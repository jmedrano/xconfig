#include <cmph.h>
#include <sstream>
#include <boost/algorithm/string.hpp>

#include "xconfig.h"

const char* XConfig::escaped_characters = "/#\\";
const XConfigNode XConfig::root_node(0);

XConfig::XConfig(XConfigConnection* conn) : conn(conn), hash(0), buckets(0)
{
	conn->connect();
	update_connection();
}

void XConfig::update_connection()
{
	const void* blob = conn->get_blob();
	if (blob) {
		const XConfigHeader* header = reinterpret_cast<const XConfigHeader*>(blob);
		hash = reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader);
		buckets = reinterpret_cast<const XConfigBucket*>(
			reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader) + header->hash_size);
		string_pool = reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader) + header->hash_size + sizeof(XConfigBucket) * header->num_buckets;
	} else {
		hash = 0;
		buckets = 0;
		string_pool = 0;
	}
}

XConfig::XConfig(std::string path, std::string socket) : XConfig(new XConfigUnixConnection(path, socket))
{
}

XConfig::~XConfig()
{
}

inline const XConfigBucket* XConfig::get_bucket(const XConfigNode& node)
{
	// idx is 1-based so that 0 means root node or none for next field
	if (!node || !buckets)
		throw XConfigNotFound();
	return &buckets[((uint32_t)node) - 1];
}

inline std::string XConfig::get_string(uint32_t offset)
{
	return std::string(&string_pool[offset]);
}

enum XConfigValueType XConfig::get_type(const XConfigNode& key)
{
	return get_bucket(key)->type;
}

struct timespec XConfig::get_mtime(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	return {bucket->mtime_secs, bucket->mtime_nsecs};
}

std::string XConfig::get_string(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type == XConfigTypeString)
		return get_string(bucket->value._string);
	throw XConfigWrongType();
}

bool XConfig::get_bool(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type == XConfigTypeBoolean)
		return bucket->value._boolean;
	throw XConfigWrongType();
}

int XConfig::get_int(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type == XConfigTypeInteger)
		return bucket->value._integer;
	throw XConfigWrongType();
}

double XConfig::get_float(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type == XConfigTypeInteger)
		return bucket->value._float;
	throw XConfigWrongType();
}

int XConfig::get_count(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type <= XConfigTypeSequence)
		return bucket->value._vectorial.size;
	throw XConfigWrongType();
}

std::vector<std::string> XConfig::get_map_keys(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type == XConfigTypeMap) {
		std::vector<std::string> ret(bucket->value._vectorial.size);
		const XConfigBucket* child = get_bucket(bucket->value._vectorial.child);
		for (auto ret_iterator = ret.begin(); child && ret_iterator != ret.end(); child = get_bucket(child->next), ++ret_iterator) {
			*ret_iterator = get_string(child->name);
		}
		return ret;
	}
	throw XConfigWrongType();
}

XConfigNode XConfig::get_node(const XConfigKeyType& key)
{
	check_connection();

	if(!hash)
		throw XConfigNotFound();

	// idx is 1-based so that 0 means root node or none for next field
	XConfigNode node = cmph_search_packed(const_cast<void*>(hash), key.c_str(), key.size()) + 1;

	if (get_key(node) != key)
		throw XConfigNotFound();

	return node;
}

XConfigNode XConfig::get_node(const std::vector<std::string>& key)
{
	return get_node(get_key(key));
}

static std::string escape_string(const std::string s)
{
	std::ostringstream ss;
	size_t p = 0;
	size_t q = s.find_first_of(XConfig::escaped_characters);
	while (q != std::string::npos) {
		ss << s.substr(p, q);
		ss << XConfig::escape_character;
		ss << s[q];
		p = q + 1;
		q = s.find_first_of(XConfig::escaped_characters, p);
	}
	ss << s.substr(p);
	return ss.str();
}

XConfigKeyType XConfig::get_key(const std::vector<std::string>& key)
{
	std::ostringstream ss;
	for (auto k = key.begin(); k != key.end(); ++k) {
		if (k != key.begin())
			ss << map_separator;
		// escape string
		size_t p = 0;
		size_t q = k->find_first_of(escaped_characters);
		while (q != std::string::npos) {
			ss << k->substr(p, q);
			ss << escape_character;
			ss << (*k)[q];
			p = q + 1;
			q = k->find_first_of(escaped_characters, p);
		}
		ss << k->substr(p);
	}
	return ss.str();
}

XConfigKeyType XConfig::get_key(const XConfigNode & node)
{
	std::string ret;	
	const XConfigBucket* bucket = get_bucket(node);
	while (bucket->parent) {
		const XConfigBucket* parent = get_bucket(bucket->parent);
		switch (parent->type) {
			case XConfigTypeMap:
				ret = map_separator + escape_string(get_string(bucket->name)) + ret;
				break;
			case XConfigTypeSequence:
				ret = sequence_separator + escape_string(get_string(bucket->name)) + ret;
				break;
			default:
				abort();
		}
		bucket = parent;
	}
	ret = escape_string(get_string(bucket->name)) + ret;
	return ret;
}

