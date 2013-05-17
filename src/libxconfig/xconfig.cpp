#include <cmph.h>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#include "xconfig.h"

using std::string;
using std::vector;

const char* XConfig::escaped_characters = "/#\\";
const XConfigNode XConfig::null_node;

XConfig::XConfig() : conn(0), hash(0), buckets(0)
{
}

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
			reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader) + header->hash_size) - 1;
		string_pool = reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader) + header->hash_size + sizeof(XConfigBucket) * header->num_buckets - 1;
	} else {
		hash = 0;
		buckets = 0;
		string_pool = 0;
	}
}

XConfig::~XConfig()
{
}

inline const XConfigBucket* XConfig::get_bucket(const XConfigNode& node)
{
	// idx is 1-based so that 0 means null node
	if (!buckets)
		throw XConfigNotConnected();
	if (!node)
		throw XConfigNotFound();
	return &buckets[((uint32_t)node)];
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
	if (bucket->type != XConfigTypeMap)
		throw XConfigWrongType();
	vector<string> ret(bucket->value._vectorial.size);
	if (bucket->value._vectorial.size > 0) {
		const XConfigBucket* child = get_bucket(bucket->value._vectorial.child);
		for (auto ret_iterator = ret.begin(); child && ret_iterator != ret.end(); child = get_bucket(child->next), ++ret_iterator) {
			*ret_iterator = get_string(child->name);
		}
	}
	return ret;
}

std::vector<XConfigNode> XConfig::get_children(const XConfigNode& key)
{
	const XConfigBucket* bucket = get_bucket(key);
	if (bucket->type != XConfigTypeMap && bucket->type != XConfigTypeSequence)
		throw XConfigWrongType();
	vector<XConfigNode> ret(bucket->value._vectorial.size);
	if (bucket->value._vectorial.size > 0) {
		XConfigNode child_node = bucket->value._vectorial.child;
		const XConfigBucket* child = get_bucket(child_node);
		for (auto ret_iterator = ret.begin(); ret_iterator != ret.end(); ++ret_iterator) {
			*ret_iterator = child_node;
			child = get_bucket(child_node);
			child_node = child->next;
		}
	}
	return ret;
}


XConfigNode XConfig::get_node_no_throw(const XConfigKeyType& key)
{
	// idx is 1-based so that 0 means null node
	XConfigNode node = cmph_search_packed(const_cast<void*>(hash), key.c_str(), key.size()) + 1;

	if (get_key(node) != key)
		node = null_node;

	return node;
}

XConfigNode XConfig::get_node_no_throw(const std::vector<std::string>& key)
{
	return get_node_no_throw(escape_key(key));
}

XConfigNode XConfig::get_node(const XConfigKeyType& key)
{
	check_connection();

	if(!hash)
		throw XConfigNotConnected();

	XConfigNode node = get_node_no_throw(key);

	if (!node)
		throw XConfigNotFound();

	return node;
}

XConfigNode XConfig::get_node(const std::vector<std::string>& key)
{
	return get_node(escape_key(key));
}

static std::ostringstream& escape_string(std::ostringstream& ss, const string& s)
{
	size_t p = 0;
	size_t q = s.find_first_of(XConfig::escaped_characters);
	while (q != string::npos) {
		ss << s.substr(p, q);
		ss << XConfig::escape_character;
		ss << s[q];
		p = q + 1;
		q = s.find_first_of(XConfig::escaped_characters, p);
	}
	ss << s.substr(p);
	return ss;
}

std::string XConfig::escape_key(const std::string& key)
{
	std::ostringstream ss;
	escape_string(ss, key);
	return ss.str();
}

XConfigKeyType XConfig::escape_key(const std::vector<std::string>& key)
{
	std::ostringstream ss;
	for (auto k = key.begin(); k != key.end(); ++k) {
		if (k != key.begin())
			ss << map_separator;
		escape_string(ss, *k);
	}
	return ss.str();
}

XConfigKeyType XConfig::get_key(const XConfigNode & node)
{
	std::ostringstream ss;
	vector<string> keys;
	const XConfigBucket* bucket = get_bucket(node);
	while (bucket->parent) {
		const XConfigBucket* parent = get_bucket(bucket->parent);
		string separator;
		switch (parent->type) {
		case XConfigTypeMap:
			keys.push_back(get_string(bucket->name));
			separator = map_separator;
			break;
		case XConfigTypeSequence:
			keys.push_back(boost::lexical_cast<string>(bucket->name));
			separator = sequence_separator;
			break;
		default:
			abort();
		}
		if (parent->parent)
			keys.push_back(separator);
		bucket = parent;
	}
	int i = 0;
	for (auto it = keys.rbegin(); it != keys.rend(); ++it, ++i) {
		// odd items need to be escaped; even items are just separators
		if (i % 2)
			ss << *it;
		else
			escape_string(ss, *it);
	}
	return ss.str();
}

XConfigNode XConfig::get_parent(const XConfigNode& node)
{
	const XConfigBucket* bucket = get_bucket(node);
	if (bucket->parent)
		return bucket->parent;
	throw XConfigWrongType();
}

std::string XConfig::get_name(const XConfigNode& node)
{
	const XConfigBucket* bucket = get_bucket(node);
	if (bucket->parent) {
		const XConfigBucket* parent = get_bucket(bucket->parent);
		switch (parent->type) {
		case XConfigTypeMap:
			return get_string(bucket->name);
			break;
		case XConfigTypeSequence:
			return boost::lexical_cast<string>(bucket->name);
			break;
		default:
			throw XConfigWrongType();
		}
	}
	// root node
	return string();
}

