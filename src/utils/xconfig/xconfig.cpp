#include <xconfig.h>
#include <xconfig_file.h>

#include <yaml.h>
#include <cmph.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>

#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>
#include <boost/program_options.hpp>
#include <iostream>

using std::string;
using boost::lexical_cast;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigHeader;
using xconfig::XConfigBucket;
using xconfig::XConfigValueType;
using xconfig::XConfigConnection;
using xconfig::FileConnection;
using xconfig::UnixConnection;
using xconfig::UnixConnectionPool;

static const int MAX_BUCKETS = 65536;
static XConfigBucket buckets[MAX_BUCKETS];
static char* keys[MAX_BUCKETS];
static int bucketIdx = 0;
static char stringPool[MAX_BUCKETS * 128];
static int stringOffset = 0;

class Dumper {
public:
	Dumper(const XConfig& xc, bool implicitYamlSeparator) : xc(xc), implicitYamlSeparator(implicitYamlSeparator) {}
	void yamlStart();
	void yamlDump(const XConfigNode& node);
	void yamlEnd();
private:
	const XConfig& xc;
	yaml_emitter_t emitter;
	yaml_event_t event;
	bool implicitYamlSeparator;

	void emitterError();
};

static void parserError(const yaml_parser_t* parser) {
	switch (parser->error) {
		case YAML_MEMORY_ERROR:
			fprintf(stderr, "Memory error: Not enough memory for emitting\n");
			break;
		case YAML_WRITER_ERROR:
			fprintf(stderr, "Writer error: %s\n", parser->problem);
			break;
		case YAML_EMITTER_ERROR:
			fprintf(stderr, "Emitter error: %s\n", parser->problem);
			break;
		default:
			/* Couldn't happen. */
			fprintf(stderr, "Internal error: %s\n", parser->problem);
			break;
	}
	abort();
}

static int yamlParseNode(yaml_parser_t* parser, const string& prefix, bool isDocumentRoot, bool isMapping) {
	yaml_event_t event;
	bool nextNodeIsKey = isMapping;
	uint32_t name = 0;
	uint32_t lastSibling = 0;
	int numBuckets = 0;
	XConfigBucket* currentBucket;
	string nextPrefix;
	int parent = isDocumentRoot ? 0 : bucketIdx;
	for (;;) {
		if (!yaml_parser_parse(parser, &event))
			parserError(parser);
		if (!isDocumentRoot) {
			if (isMapping && !nextNodeIsKey) {
				nextPrefix = !prefix.empty()
					? prefix + XConfig::MAP_SEPARATOR + XConfig::escapeKey(stringPool + name - 1)
					: (stringPool + name - 1);
			} else if (!isMapping) {
				name = numBuckets;
				nextPrefix = prefix + XConfig::SEQUENCE_SEPARATOR + lexical_cast<string>(numBuckets);
			} else {
				// this should never be used
				nextPrefix = "NaN";
			}
		}
		switch (event.type) {
			case YAML_SCALAR_EVENT:
				memcpy(stringPool + stringOffset, event.data.scalar.value, event.data.scalar.length + 1);
				if (nextNodeIsKey) {
					name = stringOffset + 1;
				} else {
					lastSibling = bucketIdx;
					keys[bucketIdx] = strdup(nextPrefix.c_str());
					currentBucket = &buckets[bucketIdx];
					bucketIdx++;
					currentBucket->name = name;
					currentBucket->parent = parent;
					currentBucket->type = XConfigValueType::TYPE_STRING;
					currentBucket->value._string = stringOffset + 1;
					currentBucket->next = bucketIdx + 1;
					name = 0;
				}
				stringOffset += event.data.scalar.length + 1;
				break;
			case YAML_MAPPING_START_EVENT:
				if (nextNodeIsKey) {
					fprintf(stderr, "complex keys not supported\n");
					abort();
				}
				lastSibling = bucketIdx;
				keys[bucketIdx] = strdup(nextPrefix.c_str());
				currentBucket = &buckets[bucketIdx];
				bucketIdx++;
				currentBucket->name = name;
				currentBucket->parent = parent;
				currentBucket->type = XConfigValueType::TYPE_MAP;
				currentBucket->value._vectorial.child = bucketIdx + 1;
				currentBucket->value._vectorial.size = yamlParseNode(parser, nextPrefix, false, true);
				currentBucket->next = bucketIdx + 1;
				if (currentBucket->value._vectorial.size == 0)
					currentBucket->value._vectorial.child = 0;
				break;
			case YAML_SEQUENCE_START_EVENT:
				if (nextNodeIsKey) {
					fprintf(stderr, "complex keys not supported\n");
					abort();
				}
				lastSibling = bucketIdx;
				keys[bucketIdx] = strdup(nextPrefix.c_str());
				currentBucket = &buckets[bucketIdx];
				bucketIdx++;
				currentBucket->name = name;
				currentBucket->parent = parent;
				currentBucket->type = XConfigValueType::TYPE_SEQUENCE;
				currentBucket->value._vectorial.child = bucketIdx + 1;
				currentBucket->value._vectorial.size = yamlParseNode(parser, nextPrefix, false, false);
				currentBucket->next = bucketIdx + 1;
				if (currentBucket->value._vectorial.size == 0)
					currentBucket->value._vectorial.child = 0;
				break;
			case YAML_DOCUMENT_START_EVENT:
				return yamlParseNode(parser, "", true, false);
			case YAML_MAPPING_END_EVENT:
				if (numBuckets > 0)
					buckets[lastSibling].next = 0;
				return numBuckets;
			case YAML_SEQUENCE_END_EVENT:
				if (numBuckets > 0)
					buckets[lastSibling].next = 0;
				return numBuckets;
			case YAML_DOCUMENT_END_EVENT:
			case YAML_STREAM_END_EVENT:
				return numBuckets;
			default:
				break;
		}
		if (!nextNodeIsKey)
			numBuckets++;
		if (isMapping)
			nextNodeIsKey = !nextNodeIsKey;
	}
}

static void yamlParse(const string& yaml_path, const string& xcPath) {
	FILE *file;
	yaml_parser_t parser;

	stringPool[stringOffset++] = 0;

	file = fopen(yaml_path.c_str(), "rb");
	assert(file);
	assert(yaml_parser_initialize(&parser));
	yaml_parser_set_input_file(&parser, file);

	yamlParseNode(&parser, "", true, false);

	yaml_parser_delete(&parser);
	fclose(file);

	cmph_io_adapter_t *source = cmph_io_vector_adapter(keys, bucketIdx);
	cmph_config_t *config = cmph_config_new(source);
	cmph_t *hash = cmph_new(config);
	cmph_config_destroy(config);

	unlink(xcPath.c_str());
	FILE* xcFile = fopen(xcPath.c_str(), "w");
	XConfigHeader header;
	header.hashSize = cmph_packed_size(hash);
	void* hashBuffer = malloc(header.hashSize);
	cmph_pack(hash, hashBuffer);
	header.numBuckets = bucketIdx;
	int totalSize = sizeof(header) + header.hashSize + sizeof(XConfigBucket) * header.numBuckets + stringOffset;
	fwrite(&header, sizeof(header), 1, xcFile);
	fwrite(hashBuffer, header.hashSize, 1, xcFile);
	fwrite(buckets, sizeof(XConfigBucket), header.numBuckets, xcFile);
	fwrite(stringPool, stringOffset, 1, xcFile);
	fflush(xcFile);
	fclose(xcFile);
	printf("generated %s file\nheader_size=%lu hash_size=%d buckets_size=%lu strings_size=%d total_size=%d\n",
			xcPath.c_str(), sizeof(header), header.hashSize,
			sizeof(XConfigBucket) * header.numBuckets, stringOffset, totalSize);
}
void Dumper::yamlStart() {
	yaml_emitter_initialize(&emitter);
	yaml_emitter_set_unicode(&emitter, 1);
	yaml_emitter_set_output_file(&emitter, stdout);
	yaml_emitter_open(&emitter);

	yaml_document_start_event_initialize(&event, NULL, NULL, NULL, implicitYamlSeparator);
	if (!yaml_emitter_emit(&emitter, &event))
		emitterError();
}
void Dumper::yamlEnd() {
	if (!implicitYamlSeparator) {
		yaml_document_end_event_initialize(&event, implicitYamlSeparator);
		if (!yaml_emitter_emit(&emitter, &event))
			emitterError();
		yaml_emitter_flush(&emitter);
	} else {
		yaml_emitter_flush(&emitter);
		printf("\n");
	}

	yaml_emitter_close(&emitter);
	yaml_emitter_delete(&emitter);
}
void Dumper::emitterError() {
	switch (emitter.error) {
		case YAML_MEMORY_ERROR:
			fprintf(stderr, "Memory error: Not enough memory for emitting\n");
			break;
		case YAML_WRITER_ERROR:
			fprintf(stderr, "Writer error: %s\n", emitter.problem);
			break;
		case YAML_EMITTER_ERROR:
			fprintf(stderr, "Emitter error: %s\n", emitter.problem);
			break;
		default:
			/* Couldn't happen. */
			fprintf(stderr, "Internal error\n");
			break;
	}
	abort();
}

static void serverThread(int connFd, int treeFd) {
	char buf[1024];
	int flags = fcntl(connFd, F_GETFL, 0);
	if (flags < 0) {
		perror("fcntl");
		return;
	}
	flags |= O_NONBLOCK;
	flags = fcntl(connFd, F_SETFL, flags);
	if (flags < 0) {
		perror("fcntl");
		return;
	}
	for (;;) {
		int rc = ::read(connFd, buf, sizeof(buf));
		if (rc == -1 && errno != EAGAIN) {
			perror("read");
			return;
		} else if (rc == 0) {
			close(connFd);
			return;
		}

		if (rc > 0) {
			printf("read %u bytes: %.*s\n", rc, rc, buf);
		} else {
			sleep(1);
		}

		char control[sizeof(struct cmsghdr)+10];
		struct msghdr msg;
		struct cmsghdr *cmsg;
		struct iovec iov;
		const string response(string(UnixConnection::PUSH_MSG) + UnixConnection::TERMINATOR);

		/* Response data */
		iov.iov_base = const_cast<void*>(reinterpret_cast<const void*>(response.c_str()));
		iov.iov_len = response.length();

		/* compose the message */
		memset(&msg, 0, sizeof(msg));
		msg.msg_iov = &iov;
		msg.msg_iovlen = 1;
		msg.msg_control = control;
		msg.msg_controllen = sizeof(control);

		/* send treeFd */
		cmsg = CMSG_FIRSTHDR(&msg);
		cmsg->cmsg_level = SOL_SOCKET;
		cmsg->cmsg_type = SCM_RIGHTS;
		cmsg->cmsg_len = CMSG_LEN(sizeof(treeFd));
		*reinterpret_cast<int *>(CMSG_DATA(cmsg)) = treeFd;

		msg.msg_controllen = cmsg->cmsg_len;

		if (sendmsg(connFd, &msg, 0) < 0) {
			perror("sendmsg error");
			close(connFd);
			return;
		}
	}
}

static void serveFile(const string& xcPath, const string& socket) {
	const int treeFd = ::open(xcPath.c_str(), O_RDONLY);
	if (treeFd < 0) {
		perror("open error");
		return;
	}
	const int fd = ::socket(AF_UNIX, SOCK_STREAM, 0);
	if (fd < 0) {
		close(treeFd);
		perror("socket error");
		return;
	}
	struct sockaddr_un addr;
	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, socket.c_str(), sizeof(addr.sun_path) - 1);
	// remove socket before bind
	::unlink(socket.c_str());
	if (::bind(fd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) < 0) {
		close(fd);
		close(treeFd);
		perror("bind error");
		return;
	}
	if (listen(fd, 5) < 0) {
		close(fd);
		close(treeFd);
		perror("listen error");
		return;
	}
	signal(SIGPIPE, SIG_IGN);
	for (;;) {
		const int connFd = ::accept(fd, NULL, NULL);
		if (connFd < 0) {
			close(fd);
			close(treeFd);
			perror("accept error");
			continue;
		}
		boost::thread newThread(serverThread, connFd, treeFd);
	}
}

void Dumper::yamlDump(const XConfigNode& node) {
	yaml_event_t event;
	switch(xc.getType(node)) {
		case XConfigValueType::TYPE_MAP: {
			yaml_mapping_start_event_initialize(&event, NULL, NULL, 1, YAML_BLOCK_MAPPING_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			auto children = xc.getChildren(node);
			for (auto child = children.begin(); child != children.end(); ++child) {
				yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_STR_TAG,
					(yaml_char_t*)xc.getName(*child).c_str(),
					-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
				if (!yaml_emitter_emit(&emitter, &event))
					emitterError();
				yamlDump(*child);
			}
			yaml_mapping_end_event_initialize(&event);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		}
		case XConfigValueType::TYPE_SEQUENCE: {
			yaml_sequence_start_event_initialize(&event, NULL, NULL, 1, YAML_BLOCK_SEQUENCE_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			auto children = xc.getChildren(node);
			for (auto child = children.begin(); child != children.end(); ++child) {
				yamlDump(*child);
			}
			yaml_sequence_end_event_initialize(&event);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		}
		case XConfigValueType::TYPE_BOOLEAN:
			yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_BOOL_TAG,
				(yaml_char_t*)(xc.getBool(node) ? "true" : "false"),
				-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		case XConfigValueType::TYPE_INTEGER:
			yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_INT_TAG,
				(yaml_char_t*)lexical_cast<string>(xc.getInt(node)).c_str(),
				-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		case XConfigValueType::TYPE_FLOAT:
			yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_FLOAT_TAG,
				(yaml_char_t*)lexical_cast<string>(xc.getFloat(node)).c_str(),
				-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		case XConfigValueType::TYPE_STRING:
			yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_STR_TAG,
				(yaml_char_t*)xc.getString(node).c_str(), -1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
			if (!yaml_emitter_emit(&emitter, &event))
				emitterError();
			break;
		default:
			fprintf(stderr, "Unknown type\n");
			break;
	}
}

int main(int argc, char** argv)
{
	// Declare the supported options.
	boost::program_options::options_description desc("Allowed options");
	desc.add_options()
		("help,h", "produce help message")
		("generate,g", boost::program_options::value<string>(), "generate xconfig file")
		("file,f", boost::program_options::value<string>(), "xconfig file")
		("key,k", boost::program_options::value<string>()->default_value(""), "key node to query")
		("server,s", "server mode")
		("socket,t", boost::program_options::value<string>()->default_value(UnixConnection::DEFAULT_SOCKET), "socket")
		("path,p", boost::program_options::value<string>()->implicit_value(""), "configuration path")
		("noind,y", "do not include yaml document indicators");

	boost::program_options::variables_map vm;
	boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
	boost::program_options::notify(vm);

	if (vm.count("help") || vm.count("generate") + vm.count("file") == 0) {
		std::cout << desc << "\n";
		return 1;
	}

	if (vm.count("generate")) {
		string yamlFile = vm["generate"].as<string>();
		string xcFile;
		if (vm.count("file")) {
			xcFile = vm["file"].as<string>();
		} else {
			xcFile = yamlFile + ".xc";
		}
		yamlParse(yamlFile, xcFile);
		return 0;
	}

	string path(vm["file"].as<string>());
	string socket(vm["socket"].as<string>());
	if (vm.count("server")) {
		serveFile(path, socket);
	} else {
		string key(vm["key"].as<string>());
		int implicitYamlSeparator = vm.count("noind");

		boost::shared_ptr<XConfigConnection> conn;
		UnixConnectionPool pool;
		if (vm.count("path")) {
			conn = pool.getConnection(path, socket);
		} else {
			conn.reset(new FileConnection(path));
		}
		XConfig xc(conn);
		Dumper dumper(xc, implicitYamlSeparator);
		XConfigNode root = xc.getNode(key);
		dumper.yamlStart();
		dumper.yamlDump(root);
		dumper.yamlEnd();
	}

	//sleep(30);

	return 0;
}

