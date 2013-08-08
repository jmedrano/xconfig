#include "YamlParser.h"

#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <yaml.h>
#include <cmph.h>
#include <string>

#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

using std::string;
using boost::lexical_cast;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigHeader;
using xconfig::XConfigBucket;
using xconfig::XConfigValueType;

YamlParser::YamlParser(string path) : path(path), bucketIdx(0), stringOffset(0), mtime{0, 0}
{
}

YamlParser::~YamlParser()
{
	for	(auto key = keys.begin(); key != keys.end(); ++key) {
		free(*key);
	}
}

const xconfig::XConfigHeader& YamlParser::getHeader() const {
	return header;
}
const xconfig::XConfigBucket* YamlParser::getBuckets() const {
	return &buckets[0];
}
size_t YamlParser::getNumBuckets() const {
	return getHeader().numBuckets;
}
const char* YamlParser::getStringPool() const {
	return &stringPool[0];
}
size_t YamlParser::getStringPoolSize() const {
	return stringPool.size();
}

const std::vector<char*>& YamlParser::getKeys() const {
	return keys;
}

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

void YamlParser::inferScalarType(XConfigBucket* bucket, const char* value, const char* tag) {
	size_t len = strlen(value);
	// interpret tag
	try {
		if (tag == NULL) {
		} else if (strcmp(tag, YAML_STR_TAG) == 0) {
			bucket->type = xconfig::TYPE_STRING;
			stringPool.insert(stringPool.end(), value, value + len + 1);
			bucket->value._string = stringOffset + 1;
			stringOffset += len + 1;
			return;
		} else if (strcmp(tag, YAML_INT_TAG) == 0) {
			bucket->type = xconfig::TYPE_INTEGER;
			bucket->value._integer = lexical_cast<long>(value);
			return;
		} else if (strcmp(tag, YAML_BOOL_TAG) == 0) {
			bucket->type = xconfig::TYPE_BOOLEAN;
			bucket->value._boolean = strcmp(value, "true") == 0;
			return;
		} else if (strcmp(tag, YAML_FLOAT_TAG) == 0) {
			bucket->type = xconfig::TYPE_FLOAT;
			bucket->value._float = lexical_cast<double>(value);
			return;
		} else if (strcmp(tag, YAML_NULL_TAG) == 0) {
			bucket->type = xconfig::TYPE_NULL;
			return;
		} else if (strcmp(tag, "!delete") == 0) {
			bucket->type = xconfig::TYPE_DELETE;
			return;
		} else if (strcmp(tag, "!expandref") == 0) {
			bucket->type = xconfig::TYPE_EXPANDREF;
			stringPool.insert(stringPool.end(), value, value + len + 1);
			bucket->value._string = stringOffset + 1;
			stringOffset += len + 1;
			return;
		} else {
			printf("unknown tag [%s]\n", tag);
		}
	} catch (boost::bad_lexical_cast) {
		// TODO log error
		printf("type error\n");
	}

	// infer type
	if (len == 0) {
		bucket->type = xconfig::TYPE_STRING;
	} else {
		if (isdigit(value[0])) {
			try {
				long int_val = lexical_cast<long>(value);
				if (lexical_cast<string>(int_val) == value) {
					printf("inferred int\n");
					bucket->type = xconfig::TYPE_INTEGER;
					bucket->value._integer = int_val;
					return;
				}
			} catch (boost::bad_lexical_cast) {
				// not an int
				try {
					double float_val = lexical_cast<double>(value);
					if (lexical_cast<string>(float_val) == value) {
						printf("inferred float\n");
						bucket->type = xconfig::TYPE_FLOAT;
						bucket->value._float = float_val;
						return;
					}
				} catch (boost::bad_lexical_cast) {
				// not a float
				}
			}
		} else if (value[0] == '.') {
			try {
				double float_val = lexical_cast<double>(value);
				if (lexical_cast<string>(float_val) == value) {
					printf("inferred float\n");
					bucket->type = xconfig::TYPE_FLOAT;
					bucket->value._float = float_val;
					return;
				}
			} catch (boost::bad_lexical_cast) {
				// not a float
			}
		} else if (strcmp(value, "null") == 0) {
			bucket->type = xconfig::TYPE_NULL;
			return;
		} else if (strcmp(value, "true") == 0) {
			bucket->type = xconfig::TYPE_BOOLEAN;
			bucket->value._boolean = true;
			return;
		} else if (strcmp(value, "false") == 0) {
			bucket->type = xconfig::TYPE_BOOLEAN;
			bucket->value._boolean = false;
			return;
		}
	}

	// string is last option
	bucket->type = xconfig::TYPE_STRING;
	stringPool.insert(stringPool.end(), value, value + len + 1);
	bucket->value._string = stringOffset + 1;
	stringOffset += len + 1;
}

int YamlParser::yamlParseNode(const string& prefix, bool isDocumentRoot, bool isMapping, int maxItems) {
	yaml_event_t event;
	bool nextNodeIsKey = isMapping;
	uint32_t name = 0;
	uint32_t lastSibling = 0;
	int numBuckets = 0;
	XConfigBucket* currentBucket;
	string nextPrefix;
	int parent = isDocumentRoot ? 0 : bucketIdx;
//printf("yamlParseNode prefix[%s] parent=%d isMapping=%d isDocumentRoot=%d maxItems=%d\n", prefix.c_str(), parent, isMapping, isDocumentRoot, maxItems);
	for (int n=0; maxItems < 0 || n < maxItems; n++) {
		if (!yaml_parser_parse(parser, &event))
			parserError(parser);
		if (!isDocumentRoot) {
			if (isMapping && !nextNodeIsKey) {
				if (prefix.empty()) {
					nextPrefix = &stringPool[name - 1];
				} else {
					nextPrefix = prefix + XConfig::MAP_SEPARATOR + XConfig::escapeKey(&stringPool[name - 1]);
				}
			} else if (!isMapping) {
				name = numBuckets;
				nextPrefix = prefix + XConfig::SEQUENCE_SEPARATOR + lexical_cast<string>(numBuckets);
			} else {
				// this should never be used
				nextPrefix = "NaN";
			}
		}
		if (maxItems == 1) {
			// TODO this is ugly
			nextPrefix = prefix;
		}
//printf("nextPrefix=[%s] prefix=[%s] isMapping=%d nextNodeIsKey=%d, isDocumentRoot=%d\n", nextPrefix.c_str(), prefix.c_str(), isMapping, nextNodeIsKey, isDocumentRoot);
		switch (event.type) {
			case YAML_SCALAR_EVENT:
assert(stringOffset == stringPool.size());
//printf("YAML_SCALAR_EVENT: stringPool.size()=%ld\n", stringPool.size());;
printf("YAML_SCALAR_EVENT: name: [%s]\n", event.data.scalar.value);
printf("YAML_SCALAR_EVENT: tag: [%s]\n", event.data.scalar.tag);
//printf("YAML_SCALAR_EVENT: stringPool.size()=%ld\n", stringPool.size());;
				if (nextNodeIsKey) {
					stringPool.insert(stringPool.end(), (char*)event.data.scalar.value, (char*)event.data.scalar.value + event.data.scalar.length + 1);
					name = stringOffset + 1;
					stringOffset += event.data.scalar.length + 1;
				} else {
					lastSibling = bucketIdx;
//printf("insert scalar bucket bucketIdx=%ld, buckets.size()=%d, key=[%s], value=[%s]\n", bucketIdx, buckets.size(), nextPrefix.c_str(), &stringPool[stringOffset]);
					currentBucket = insertBucket(nextPrefix);
					currentBucket->name = name;
					currentBucket->parent = parent;
					// TODO infer type
					inferScalarType(currentBucket, (char*)event.data.scalar.value, (char*)event.data.scalar.tag);
					//currentBucket->type = XConfigValueType::TYPE_STRING;
					//currentBucket->value._string = stringOffset + 1;
					currentBucket->next = bucketIdx + 1;
					name = 0;
				}
				break;
			case YAML_MAPPING_START_EVENT:
				if (nextNodeIsKey) {
					fprintf(stderr, "complex keys not supported\n");
					abort();
				}
				lastSibling = bucketIdx;
//printf("insert map bucket bucketIdx=%ld, buckets.size()=%d\n", bucketIdx, buckets.size());
				currentBucket = insertBucket(nextPrefix);
				currentBucket->name = name;
				currentBucket->parent = parent;
				currentBucket->type = XConfigValueType::TYPE_MAP;
				currentBucket->value._vectorial.child = bucketIdx + 1;
				currentBucket->value._vectorial.size = yamlParseNode(nextPrefix, false, true);
				currentBucket->next = bucketIdx + 1;
//printf("insert map bucket child=%ld, size=%d, next=%ld\n", currentBucket->value._vectorial.child, currentBucket->value._vectorial.size, currentBucket->next);
				if (currentBucket->value._vectorial.size == 0)
					currentBucket->value._vectorial.child = 0;
				break;
			case YAML_SEQUENCE_START_EVENT:
				if (nextNodeIsKey) {
					size_t firstBucketId = 0;
					string overrideKey;
					size_t previousName = 0;
					name = 0;
				//printf("override start\n");
					for(bool searchingKeys=true; searchingKeys;) {
						yaml_event_delete(&event);
						if (!yaml_parser_parse(parser, &event))
							parserError(parser);
						switch (event.type) {
							case YAML_SCALAR_EVENT:
								assert(stringOffset == stringPool.size());
								//printf("YAML_SCALAR_EVENT: stringPool.size()=%ld\n", stringPool.size());;
								stringPool.insert(stringPool.end(), (char*)event.data.scalar.value, (char*)event.data.scalar.value + event.data.scalar.length + 1);
								//printf("YAML_SCALAR_EVENT: name: [%s] %ld\n", &stringPool[stringOffset], event.data.scalar.length);
								//printf("YAML_SCALAR_EVENT: stringPool.size()=%ld\n", stringPool.size());;
								previousName = name;
								name = stringOffset + 1;
								stringOffset += event.data.scalar.length + 1;
								break;
							case YAML_SEQUENCE_END_EVENT:
								searchingKeys = false;
								//printf("override end keys\n");
								continue;
							default:
								fprintf(stderr, "Unexpected event\n");
								abort();
						}
						if (previousName) {
							// there was another key on the previous iteration
							// insert a map node for it
							//printf("override map key=[%s] name=[%s]\n", overrideKey.c_str(), &stringPool[previousName - 1]);
							currentBucket = insertBucket(overrideKey);
							currentBucket->type = xconfig::TYPE_MAP;
							currentBucket->name = previousName;
							currentBucket->value._vectorial.size = 1;
							currentBucket->value._vectorial.child = bucketIdx + 1;
							currentBucket->next = 0;
							currentBucket->parent = bucketIdx - 1;
							overrideKey += XConfig::MAP_SEPARATOR;
						}
						overrideKey += &stringPool[name - 1];
						if (!firstBucketId)
							firstBucketId = bucketIdx;
					}
					if (overrideKey.empty()) {
						fprintf(stderr, "Empty override key\n");
						abort();
					}
					// parse value
					//printf("override value key=[%s] name=[%s]\n", overrideKey.c_str(), &stringPool[name - 1]);
					size_t valueIdx = bucketIdx;
					yamlParseNode(overrideKey, true, false, 1);
					currentBucket = &buckets[valueIdx];
					// fix value node
					currentBucket->name = name;
					if (firstBucketId) {
						currentBucket->parent = bucketIdx - 1;
					} else {
						currentBucket->parent = parent;
						firstBucketId = valueIdx;
					}
					buckets[firstBucketId].next = bucketIdx + 1;

					nextNodeIsKey = false; // it'll be flipped to true on outer loop
					//printf("override end\n");
					break;
				}
				lastSibling = bucketIdx;
				currentBucket = insertBucket(nextPrefix);
				currentBucket->name = name;
				currentBucket->parent = parent;
				currentBucket->type = XConfigValueType::TYPE_SEQUENCE;
				currentBucket->value._vectorial.child = bucketIdx + 1;
				currentBucket->value._vectorial.size = yamlParseNode(nextPrefix, false, false);
				currentBucket->next = bucketIdx + 1;
				if (currentBucket->value._vectorial.size == 0)
					currentBucket->value._vectorial.child = 0;
				break;
			case YAML_DOCUMENT_START_EVENT:
				yaml_event_delete(&event);
				return yamlParseNode("", true, false);
			case YAML_MAPPING_END_EVENT:
//printf("YAML_MAPPING_END_EVENT\n");
				if (numBuckets > 0)
					buckets[lastSibling].next = 0;
				yaml_event_delete(&event);
				return numBuckets;
			case YAML_SEQUENCE_END_EVENT:
//printf("YAML_SEQUENCE_END_EVENT\n");
				if (numBuckets > 0)
					buckets[lastSibling].next = 0;
				yaml_event_delete(&event);
				return numBuckets;
			case YAML_DOCUMENT_END_EVENT:
			case YAML_STREAM_END_EVENT:
				yaml_event_delete(&event);
				return numBuckets;
			default:
				break;
		}
		if (!nextNodeIsKey)
			numBuckets++;
		if (isMapping)
			nextNodeIsKey = !nextNodeIsKey;
		yaml_event_delete(&event);
	}
//printf("maxItems reached\n");
	return numBuckets;
}

bool YamlParser::parse() {
	int fd = ::open(path.c_str(), O_RDONLY);
	if (fd < 0) {
		printf("can't open %s\n", path.c_str());
		// TODO throw FileNotFound
		return false;
	}
	{
		// check if mtime changed
		struct stat st;
		fstat(fd, &st);
		if (st.st_mtime == mtime.tv_sec && st.st_mtim.tv_nsec == mtime.tv_nsec) {
			::close(fd);
			return false;
		}
		mtime = {st.st_mtime, st.st_mtim.tv_nsec};
	}

	FILE *file;
	parser = new yaml_parser_t;
	buckets.clear();
	keys.clear();
	stringPool.clear();
	buckets.reserve(RESERVE_BUCKETS);
	keys.reserve(RESERVE_BUCKETS);
	stringPool.reserve(RESERVE_STRINGPOOL);
	bucketIdx = 0;
	stringPool.push_back(0);
	stringOffset = 1;

	file = fdopen(fd, "rb");
	assert(file);
	assert(yaml_parser_initialize(parser));
	yaml_parser_set_input_file(parser, file);

	yamlParseNode("", true, false);

	yaml_parser_delete(parser);
	delete(parser);
	parser = NULL;
	fclose(file);
	::close(fd);

//printf("keys.size()=%ld\n", keys.size());
int nkey = 0;
for	(auto key = keys.begin(); key != keys.end(); ++key) {
	//printf(" keys[%d]=[%s]\n", nkey, *key);
	nkey++;
}
	cmph_io_adapter_t *source = cmph_io_vector_adapter(&keys[0], keys.size());
//printf("source=%p\n", source);
	cmph_config_t *config = cmph_config_new(source);
	cmph_config_set_algo(config, CMPH_CHM);
//printf("config=%p\n", config);
	cmph_t *hash_serialization = cmph_new(config);
//printf("hash_serialization=%p\n", hash_serialization);
	cmph_config_destroy(config);

	size_t hashSize = cmph_packed_size(hash_serialization);
	cmph_destroy(hash_serialization);
	cmph_io_vector_adapter_destroy(source);
	size_t numBuckets = bucketIdx;
	totalSize = sizeof(header) + hashSize + sizeof(XConfigBucket) * numBuckets + stringOffset;
	size_t offset = 0;

	header.hashSize = hashSize;
	header.numBuckets = numBuckets;
	offset += sizeof(header);
	assert(offset < totalSize);
//	cmph_pack(hash_serialization, blob + offset);
	offset += hashSize;
	assert(offset < totalSize);

	offset += sizeof(XConfigBucket) * numBuckets;
	assert(offset < totalSize);
	assert(offset + stringOffset == totalSize);
	assert(stringOffset == stringPool.size());
	assert(bucketIdx == buckets.size());
	assert(keys.size() == buckets.size());

	return true;
}

xconfig::XConfigBucket* YamlParser::insertBucket(const std::string& key)
{
	keys.push_back(strdup(key.c_str()));
	buckets.resize(buckets.size()+1);
	bucketIdx++;
	XConfigBucket* bucket = &buckets.back();
	bucket->mtimeSecs = mtime.tv_sec;
	bucket->mtimeNsecs = mtime.tv_nsec;
	return bucket;
}
