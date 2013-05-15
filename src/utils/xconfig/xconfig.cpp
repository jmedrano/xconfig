#include <xconfig.h>
#include <xconfig_file.h>
#include <yaml.h>
#include <cmph.h>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>

#include <iostream>

using std::string;
using boost::lexical_cast;

static const int MAX_BUCKETS = 65536;
static XConfigBucket buckets[MAX_BUCKETS];
static char* keys[MAX_BUCKETS];
static int bucket_idx = 0;
static char string_pool[MAX_BUCKETS * 128];
static int string_offset = 0;

static void parser_error(yaml_parser_t& parser) {
	switch (parser.error) {
	case YAML_MEMORY_ERROR:
		fprintf(stderr, "Memory error: Not enough memory for emitting\n");
		break;
	case YAML_WRITER_ERROR:
		fprintf(stderr, "Writer error: %s\n", parser.problem);
		break;
	case YAML_EMITTER_ERROR:
		fprintf(stderr, "Emitter error: %s\n", parser.problem);
		break;
	default:
		/* Couldn't happen. */
		fprintf(stderr, "Internal error: %s\n", parser.problem);
		break;
	}
	abort();
}

static int yaml_parse_node(yaml_parser_t& parser, const string& prefix, bool is_document_root, bool is_mapping) {
	yaml_event_t event;
	bool next_node_is_key = is_mapping;
	uint32_t name = 0;
	uint32_t last_sibling = 0;
	int num_buckets = 0;
	XConfigBucket* current_bucket;
	string next_prefix;
	int parent = is_document_root ? 0 : bucket_idx;
	for (;;) {
		if (!yaml_parser_parse(&parser, &event))
			parser_error(parser);
		if (!is_document_root) {
			if (is_mapping && !next_node_is_key) {
				next_prefix = !prefix.empty()
					? prefix + XConfig::map_separator + XConfig::escape_key(string_pool + name - 1)
					: (string_pool + name - 1);
			} else if (!is_mapping) {
				name = num_buckets;
				next_prefix = prefix + XConfig::sequence_separator + lexical_cast<string>(num_buckets);
			} else {
				// this should never be used
				next_prefix = "NaN";
			}
		}
		switch (event.type) {
		case YAML_SCALAR_EVENT:
			memcpy(string_pool + string_offset, event.data.scalar.value, event.data.scalar.length + 1);
			if (next_node_is_key) {
				name = string_offset + 1;
			} else {
				last_sibling = bucket_idx;
				keys[bucket_idx] = strdup(next_prefix.c_str());
				current_bucket = &buckets[bucket_idx];
				bucket_idx++;
				current_bucket->name = name;
				current_bucket->parent = parent;
				current_bucket->type = XConfigTypeString;
				current_bucket->value._string = string_offset + 1;
				current_bucket->next = bucket_idx + 1;
				name = 0;
			}
			string_offset += event.data.scalar.length + 1;
			break;
		case YAML_MAPPING_START_EVENT:
			if (next_node_is_key) {
				fprintf(stderr, "complex keys not supported\n");
				abort();
			}
			last_sibling = bucket_idx;
			keys[bucket_idx] = strdup(next_prefix.c_str());
			current_bucket = &buckets[bucket_idx];
			bucket_idx++;
			current_bucket->name = name;
			current_bucket->parent = parent;
			current_bucket->type = XConfigTypeMap;
			current_bucket->value._vectorial.child = bucket_idx + 1;
			current_bucket->value._vectorial.size = yaml_parse_node(parser, next_prefix, false, true);
			current_bucket->next = bucket_idx + 1;
			if (current_bucket->value._vectorial.size == 0)
				current_bucket->value._vectorial.child = 0;
			break;
		case YAML_SEQUENCE_START_EVENT:
			if (next_node_is_key) {
				fprintf(stderr, "complex keys not supported\n");
				abort();
			}
			last_sibling = bucket_idx;
			keys[bucket_idx] = strdup(next_prefix.c_str());
			current_bucket = &buckets[bucket_idx];
			bucket_idx++;
			current_bucket->name = name;
			current_bucket->parent = parent;
			current_bucket->type = XConfigTypeSequence;
			current_bucket->value._vectorial.child = bucket_idx + 1;
			current_bucket->value._vectorial.size = yaml_parse_node(parser, next_prefix, false, false);
			current_bucket->next = bucket_idx + 1;
			if (current_bucket->value._vectorial.size == 0)
				current_bucket->value._vectorial.child = 0;
			break;
		case YAML_DOCUMENT_START_EVENT:
			return yaml_parse_node(parser, "", true, false);
		case YAML_MAPPING_END_EVENT:
			if (num_buckets > 0)
				buckets[last_sibling].next = 0;
			return num_buckets;
		case YAML_SEQUENCE_END_EVENT:
			if (num_buckets > 0)
				buckets[last_sibling].next = 0;
			return num_buckets;
		case YAML_DOCUMENT_END_EVENT:
		case YAML_STREAM_END_EVENT:
			return num_buckets;
		default:
			break;
		}
		if (!next_node_is_key)
			num_buckets++;
		if (is_mapping)
			next_node_is_key = !next_node_is_key;
	}
}

static void yaml_parse(const string& yaml_path, const string& xc_path) {
	FILE *file;
	yaml_parser_t parser;

	string_pool[string_offset++] = 0;

	file = fopen(yaml_path.c_str(), "rb");
	assert(file);
	assert(yaml_parser_initialize(&parser));
	yaml_parser_set_input_file(&parser, file);

	yaml_parse_node(parser, "", true, false);

	yaml_parser_delete(&parser);
	fclose(file);

	cmph_io_adapter_t *source = cmph_io_vector_adapter(keys, bucket_idx);
	cmph_config_t *config = cmph_config_new(source);
	cmph_t *hash = cmph_new(config);
	cmph_config_destroy(config);

	unlink(xc_path.c_str());
	FILE* xc_file = fopen(xc_path.c_str(), "w");
	XConfigHeader header;
	header.hash_size = cmph_packed_size(hash);
	void* hash_buffer = malloc(header.hash_size);
	cmph_pack(hash, hash_buffer);
	header.num_buckets = bucket_idx;
	int total_size = sizeof(header) + header.hash_size + sizeof(XConfigBucket) * header.num_buckets + string_offset;
	fwrite(&header, sizeof(header), 1, xc_file);
	fwrite(hash_buffer, header.hash_size, 1, xc_file);
	fwrite(buckets, sizeof(XConfigBucket), header.num_buckets, xc_file);
	fwrite(string_pool, string_offset, 1, xc_file);
	fflush(xc_file);
	fclose(xc_file);
	printf("generated %s file\nheader_size=%lu hash_size=%d buckets_size=%lu strings_size=%d total_size=%d\n",
			xc_path.c_str(), sizeof(header), header.hash_size,
			sizeof(XConfigBucket) * header.num_buckets, string_offset, total_size);
}
static void emitter_error(yaml_emitter_t& emitter) {
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

static void yaml_dump(XConfig& xc, yaml_emitter_t& emitter, XConfigNode node) {
	yaml_event_t event;
	switch(xc.get_type(node)) {
	case XConfigTypeMap:
		yaml_mapping_start_event_initialize(&event, NULL, NULL, 1, YAML_BLOCK_MAPPING_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		{
			auto children = xc.get_children(node);
			for (auto child = children.begin(); child != children.end(); ++child) {
				yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_STR_TAG,
					(yaml_char_t*)xc.get_name(*child).c_str(),
					-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
				if (!yaml_emitter_emit(&emitter, &event))
					emitter_error(emitter);
				yaml_dump(xc, emitter, *child);
			}
		}
		yaml_mapping_end_event_initialize(&event);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		break;
	case XConfigTypeSequence:
		yaml_sequence_start_event_initialize(&event, NULL, NULL, 1, YAML_BLOCK_SEQUENCE_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		{
			auto children = xc.get_children(node);
			for (auto child = children.begin(); child != children.end(); ++child) {
				yaml_dump(xc, emitter, *child);
			}
		}
		yaml_sequence_end_event_initialize(&event);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		break;
	case XConfigTypeBoolean:
		yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_BOOL_TAG,
			(yaml_char_t*)(xc.get_bool(node) ? "true" : "false"),
			-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		break;
	case XConfigTypeInteger:
		yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_INT_TAG,
			(yaml_char_t*)lexical_cast<string>(xc.get_int(node)).c_str(),
			-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		break;
	case XConfigTypeFloat:
		yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_FLOAT_TAG,
			(yaml_char_t*)lexical_cast<string>(xc.get_float(node)).c_str(),
			-1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		break;
	case XConfigTypeString:
		yaml_scalar_event_initialize(&event, NULL, (yaml_char_t*)YAML_STR_TAG,
			(yaml_char_t*)xc.get_string(node).c_str(), -1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
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
		("noind,y", "do not include yaml document indicators")
		;

	boost::program_options::variables_map vm;
	boost::program_options::store(boost::program_options::parse_command_line(argc, argv, desc), vm);
	boost::program_options::notify(vm);

	if (vm.count("help") || vm.count("generate") + vm.count("file") == 0) {
		std::cout << desc << "\n";
		return 1;
	}

	if (vm.count("generate")) {
		string yaml_file = vm["generate"].as<string>();
		string xc_file;
		if (vm.count("file")) {
			xc_file = vm["file"].as<string>();
		} else {
			xc_file = yaml_file + ".xc";
		}
		yaml_parse(yaml_file, xc_file);
		return 0;
	}

	string key(vm["key"].as<string>());
	string path(vm["file"].as<string>());
	int implicit_yaml_separator = vm.count("noind");

	yaml_emitter_t emitter;
	yaml_event_t event;

	XConfig xc(new XConfigFileConnection(path));
	XConfigNode root = xc.get_node(key);
	yaml_emitter_initialize(&emitter);
	yaml_emitter_set_unicode(&emitter, 1);
	yaml_emitter_set_output_file(&emitter, stdout);
	yaml_emitter_open(&emitter);

	yaml_document_start_event_initialize(&event, NULL, NULL, NULL, implicit_yaml_separator);
	if (!yaml_emitter_emit(&emitter, &event))
		emitter_error(emitter);

	yaml_dump(xc, emitter, root);
	if (!implicit_yaml_separator) {
		yaml_document_end_event_initialize(&event, implicit_yaml_separator);
		if (!yaml_emitter_emit(&emitter, &event))
			emitter_error(emitter);
		yaml_emitter_flush(&emitter);
	} else {
		yaml_emitter_flush(&emitter);
		printf("\n");
	}

	yaml_emitter_close(&emitter);
	yaml_emitter_delete(&emitter);

	return 0;
}

