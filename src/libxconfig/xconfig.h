#ifndef LIBXCONFIG_XCONFIG_H_
#define LIBXCONFIG_XCONFIG_H_

#include <time.h>
#include <string>
#include <vector>
#include <boost/scoped_ptr.hpp>

#include "xconfig_file.h"
#include "xconfig_connection.h"

namespace xconfig {

class XConfigNode;

class XConfig {
public:
	static const char MAP_SEPARATOR = '/';
	static const char SEQUENCE_SEPARATOR = '#';
	static const char ESCAPE_CHARACTER = '\\';
	static const char* ESCAPED_CHARACTERS;
	static const XConfigNode NULL_NODE;

	explicit XConfig(const boost::shared_ptr<XConfigConnection>& conn, bool autoReload = true);
	~XConfig();
	void connect();
	void close();

	// methods working with keys as unescaped string vectors
	enum XConfigValueType getType(const std::vector<std::string>& key) const;
	enum XConfigValueType getType(const std::vector<std::string>& key);
	struct timespec getMtime(const std::vector<std::string>& key) const;
	struct timespec getMtime(const std::vector<std::string>& key);
	bool isScalar(const std::vector<std::string>& key) const;
	bool isScalar(const std::vector<std::string>& key);
	bool isMap(const std::vector<std::string>& key) const;
	bool isMap(const std::vector<std::string>& key);
	bool isSequence(const std::vector<std::string>& key) const;
	bool isSequence(const std::vector<std::string>& key);
	std::string getString(const std::vector<std::string>& key) const;
	std::string getString(const std::vector<std::string>& key);
	bool getBool(const std::vector<std::string>& key) const;
	bool getBool(const std::vector<std::string>& key);
	int getInt(const std::vector<std::string>& key) const;
	int getInt(const std::vector<std::string>& key);
	double getFloat(const std::vector<std::string>& key) const;
	double getFloat(const std::vector<std::string>& key);
	int getCount(const std::vector<std::string>& key) const;
	int getCount(const std::vector<std::string>& key);
	std::vector<std::string> getMapKeys(const std::vector<std::string>& key) const;
	std::vector<std::string> getMapKeys(const std::vector<std::string>& key);

	// methods working with keys as escaped strings
	enum XConfigValueType getType(const std::string& key) const;
	enum XConfigValueType getType(const std::string& key);
	struct timespec getMtime(const std::string& key) const;
	struct timespec getMtime(const std::string& key);
	bool isScalar(const std::string& key) const;
	bool isScalar(const std::string& key);
	bool isMap(const std::string& key) const;
	bool isMap(const std::string& key);
	bool isSequence(const std::string& key) const;
	bool isSequence(const std::string& key);
	std::string getString(const std::string& key) const;
	std::string getString(const std::string& key);
	bool getBool(const std::string& key) const;
	bool getBool(const std::string& key);
	int getInt(const std::string& key) const;
	int getInt(const std::string& key);
	double getFloat(const std::string& key) const;
	double getFloat(const std::string& key);
	int getCount(const std::string& key) const;
	int getCount(const std::string& key);
	std::vector<std::string> getMapKeys(const std::string& key) const;
	std::vector<std::string> getMapKeys(const std::string& key);

	// methods working with node objects
	// configuration updates invalidate node objects
	enum XConfigValueType getType(const XConfigNode& key) const;
	struct timespec getMtime(const XConfigNode& key) const;
	bool isScalar(const XConfigNode& key) const;
	bool isMap(const XConfigNode& key) const;
	bool isSequence(const XConfigNode& key) const;
	std::string getString(const XConfigNode& key) const;
	bool getBool(const XConfigNode& key) const;
	int getInt(const XConfigNode& key) const;
	double getFloat(const XConfigNode& key) const;
	int getCount(const XConfigNode& key) const;
	std::vector<std::string> getMapKeys(const XConfigNode& key) const;
	std::string getName(const XConfigNode& key) const;

	// methods for transforming keys into nodes
	XConfigNode getNode(const std::string& key) const;
	XConfigNode getNode(const std::string& key);
	XConfigNode getNode(const std::vector<std::string>& key) const;
	XConfigNode getNode(const std::vector<std::string>& key);
	XConfigNode getNodeNoThrow(const std::string& key) const;
	XConfigNode getNodeNoThrow(const std::vector<std::string>& key) const;
	std::string getKey(const XConfigNode& node) const;

	// methods for tree iteration
	XConfigNode getParent(const XConfigNode& key) const;
	std::vector<XConfigNode> getChildren(const XConfigNode& key) const;

	// methods for escaping keys
	static std::string escapeKey(const std::vector<std::string>& key);
	static std::string escapeKey(const std::string& key);

	bool reload();

private:
	const boost::shared_ptr<XConfigConnection> conn;
	const void* hash;
	const XConfigBucket* buckets;
	const char* stringPool;
	const bool autoReload;

	const XConfigBucket* getBucket(const XConfigNode& key) const;
	std::string getString(uint32_t offset) const;
	void doReload();
};

class XConfigNode {
	friend class XConfig;
private:
	// bucketIdx == 0 => root node for parent, none for next field.
	// bucketIdx == 1 => first bucket
	uint32_t bucketIdx;
	// private so only available from XConfig
	XConfigNode(uint32_t b) : bucketIdx(b) { }
	operator uint32_t() const { return bucketIdx; }
public:
	XConfigNode() : bucketIdx(0) { }
};

} // namespace xconfig

#include "xconfig-inl.h"

#endif // LIBXCONFIG_XCONFIG_H_
