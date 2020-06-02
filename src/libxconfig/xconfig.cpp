#include <cmph.h>
#include <string>
#include <vector>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#include "xconfig.h"

using std::string;
using std::vector;
using boost::shared_ptr;

namespace xconfig {

const char* XConfig::ESCAPED_CHARACTERS = "/#\\";
const XConfigNode XConfig::NULL_NODE;

XConfig::XConfig(const shared_ptr<XConfigConnection>& conn, bool autoReload) : conn(conn), hash(0), buckets(0), autoReload(autoReload)
{
	conn->connect();
	applyReload();
}

bool XConfig::reload() {
	if (conn->connect()) {
		applyReload();
		return true;
	}
	return false;
}

void XConfig::close()
{
	map.reset();
	conn->close();
	hash = 0;
	buckets = 0;
	stringPool = 0;
}

void XConfig::applyReload()
{
	map = conn->getMap();
	const void* blob = map ? map->getBlob() : 0;
	if (blob) {
		const XConfigHeader* header = reinterpret_cast<const XConfigHeader*>(blob);
		hash = reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader);
		buckets = reinterpret_cast<const XConfigBucket*>(
			reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader) + header->hashSize) - 1;
		stringPool = reinterpret_cast<const char*>(blob) + sizeof(XConfigHeader)
			+ header->hashSize + sizeof(XConfigBucket) * header->numBuckets - 1;
	} else {
		hash = 0;
		buckets = 0;
		stringPool = 0;
	}
}

XConfig::~XConfig()
{
}

inline const XConfigBucket* XConfig::getBucket(const XConfigNode& node) const
{
	// idx is 1-based so that 0 means null node
	if (!buckets)
		throw XConfigNotConnected();
	if (!node)
		throw XConfigNotFound();
	assert(node.xc == this);
	return &buckets[node.getIdx()];
}

inline std::string XConfig::getString(uint32_t offset) const
{
	return std::string(&stringPool[offset]);
}

enum XConfigValueType XConfig::getType(const XConfigNode& node) const
{
	enum XConfigValueType type = getBucket(node)->type;
	// hide implementation details for expandenv and show it as a regular string
	return type == XConfigValueType::TYPE_EXPANDENV ? XConfigValueType::TYPE_STRING : type;
}

struct timespec XConfig::getMtime(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	return {bucket->mtimeSecs, bucket->mtimeNsecs}; // NOLINT(readability/braces)
}

uint64_t XConfig::getChecksum(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
        uint64_t checksum = bucket->mtimeSecs;
        return (checksum << 32) | bucket->mtimeNsecs;
}

std::string XConfig::getString(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->type == XConfigValueType::TYPE_STRING)
		return getString(bucket->value._string);
	else if (bucket->type == XConfigValueType::TYPE_EXPANDENV) {
		char * var = std::getenv(getString(bucket->value._string).c_str());
		return var ? var : "";
	}
	throw XConfigWrongType();
}

bool XConfig::getBool(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->type == XConfigValueType::TYPE_BOOLEAN)
		return bucket->value._boolean;
	throw XConfigWrongType();
}

int64_t XConfig::getInt(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->type == XConfigValueType::TYPE_INTEGER)
		return bucket->value._integer;
	if (bucket->type == XConfigValueType::TYPE_FLOAT)
		return static_cast<int64_t>(bucket->value._float);
	throw XConfigWrongType();
}

double XConfig::getFloat(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->type == XConfigValueType::TYPE_FLOAT)
		return bucket->value._float;
	if (bucket->type == XConfigValueType::TYPE_INTEGER)
		return static_cast<double>(bucket->value._integer);
	throw XConfigWrongType();
}

int XConfig::getCount(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (!xconfig::isScalar(bucket->type))
		return bucket->value._vectorial.size;
	throw XConfigWrongType();
}

std::vector<std::string> XConfig::getMapKeys(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->type != XConfigValueType::TYPE_MAP)
		throw XConfigWrongType();
	vector<string> ret(bucket->value._vectorial.size);
	if (bucket->value._vectorial.size > 0) {
		const XConfigBucket* child = getBucket(XConfigNode(this, bucket->value._vectorial.child));
		for (auto retIterator = ret.begin();
				child && retIterator != ret.end();
				child = getBucket(XConfigNode(this, child->next)), ++retIterator) {
			*retIterator = getString(child->name);
			if (!child->next)
				break;
		}
	}
	return ret;
}

std::vector<XConfigNode> XConfig::getChildren(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (xconfig::isScalar(bucket->type))
		throw XConfigWrongType();
	vector<XConfigNode> ret(bucket->value._vectorial.size);
	if (bucket->value._vectorial.size > 0) {
		XConfigNode childNode(this, bucket->value._vectorial.child);
		const XConfigBucket* child = getBucket(childNode);
		for (auto retIterator = ret.begin(); retIterator != ret.end(); ++retIterator) {
			*retIterator = childNode;
			child = getBucket(childNode);
			childNode = XConfigNode(this, child->next);
		}
	}
	return ret;
}


XConfigNode XConfig::getNodeNoThrow(const std::string& key) const
{
	if (!hash)
		throw XConfigNotConnected();

	// idx is 1-based so that 0 means null node
	XConfigNode node = XConfigNode(this, cmph_search_packed(const_cast<void*>(hash), key.c_str(), key.size()) + 1);

	if (getKey(node) != key)
		node = NULL_NODE;

	return node;
}

XConfigNode XConfig::getNodeNoThrow(const std::vector<std::string>& key) const
{
	if(!hash)
		return NULL_NODE;

	return getNodeNoThrow(escapeKey(key));
}

XConfigNode XConfig::getNode(const std::string& key) const
{
	if(!hash)
		throw XConfigNotConnected(key);

	XConfigNode node = getNodeNoThrow(key);

	if (!node)
		throw XConfigNotFound(key);

	return node;
}

XConfigNode XConfig::getNode(const std::vector<std::string>& key) const
{
	return getNode(escapeKey(key));
}

XConfigNode XConfig::getNode(const std::string& key)
{
	mightReload();

	if(!hash)
		throw XConfigNotConnected(key);

	XConfigNode node = getNodeNoThrow(key);

	if (!node)
		throw XConfigNotFound(key);

	return node;
}

XConfigNode XConfig::getNode(const std::vector<std::string>& key)
{
	return getNode(escapeKey(key));
}

static std::ostringstream& escapeString(std::ostringstream& ss, const string& s)
{
	size_t p = 0;
	size_t q = s.find_first_of(XConfig::ESCAPED_CHARACTERS);
	while (q != string::npos) {
		ss << s.substr(p, q);
		ss << XConfig::ESCAPE_CHARACTER;
		ss << s[q];
		p = q + 1;
		q = s.find_first_of(XConfig::ESCAPED_CHARACTERS, p);
	}
	ss << s.substr(p);
	return ss;
}

std::string XConfig::escapeKey(const std::string& key)
{
	std::ostringstream ss;
	escapeString(ss, key);
	return ss.str();
}

std::string XConfig::escapeKey(const std::vector<std::string>& key)
{
	std::ostringstream ss;
	for (auto k = key.begin(); k != key.end(); ++k) {
		if (k != key.begin())
			ss << MAP_SEPARATOR;
		escapeString(ss, *k);
	}
	return ss.str();
}

std::string XConfig::getKey(const XConfigNode & node) const
{
	std::ostringstream ss;
	vector<string> keys;
	const XConfigBucket* bucket = getBucket(node);
	while (bucket->parent) {
		const XConfigBucket* parent = getBucket(XConfigNode(this, bucket->parent));
		string separator;
		switch (parent->type) {
			case XConfigValueType::TYPE_MAP:
				keys.push_back(getString(bucket->name));
				separator = MAP_SEPARATOR;
				break;
			case XConfigValueType::TYPE_SEQUENCE:
				keys.push_back(boost::lexical_cast<string>(bucket->name));
				separator = SEQUENCE_SEPARATOR;
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
			escapeString(ss, *it);
	}
	return ss.str();
}

XConfigNode XConfig::getParent(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->parent)
		return XConfigNode(this, bucket->parent);
	throw XConfigWrongType();
}

std::string XConfig::getName(const XConfigNode& node) const
{
	const XConfigBucket* bucket = getBucket(node);
	if (bucket->parent) {
		const XConfigBucket* parent = getBucket(XConfigNode(this, bucket->parent));
		switch (parent->type) {
			case XConfigValueType::TYPE_MAP:
				return getString(bucket->name);
				break;
			case XConfigValueType::TYPE_SEQUENCE:
				return boost::lexical_cast<string>(bucket->name);
				break;
			default:
				throw XConfigWrongType();
		}
	}
	// root node
	return string();
}

} // namespace xconfig
