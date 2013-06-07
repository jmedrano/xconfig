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
	static const char map_separator = '/';
	static const char sequence_separator = '#';
	static const char escape_character = '\\';
	static const char* escaped_characters;
	static const XConfigNode null_node;

	explicit XConfig(const boost::shared_ptr<XConfigConnection>& conn, bool auto_reload = true);
	~XConfig();
	void connect();
	void close();

	// methods working with keys as unescaped string vectors
	enum XConfigValueType get_type(const std::vector<std::string>& key) const;
	enum XConfigValueType get_type(const std::vector<std::string>& key);
	struct timespec get_mtime(const std::vector<std::string>& key) const;
	struct timespec get_mtime(const std::vector<std::string>& key);
	bool is_scalar(const std::vector<std::string>& key) const;
	bool is_scalar(const std::vector<std::string>& key);
	bool is_map(const std::vector<std::string>& key) const;
	bool is_map(const std::vector<std::string>& key);
	bool is_sequence(const std::vector<std::string>& key) const;
	bool is_sequence(const std::vector<std::string>& key);
	std::string get_string(const std::vector<std::string>& key) const;
	std::string get_string(const std::vector<std::string>& key);
	bool get_bool(const std::vector<std::string>& key) const;
	bool get_bool(const std::vector<std::string>& key);
	int get_int(const std::vector<std::string>& key) const;
	int get_int(const std::vector<std::string>& key);
	double get_float(const std::vector<std::string>& key) const;
	double get_float(const std::vector<std::string>& key);
	int get_count(const std::vector<std::string>& key) const;
	int get_count(const std::vector<std::string>& key);
	std::vector<std::string> get_map_keys(const std::vector<std::string>& key) const;
	std::vector<std::string> get_map_keys(const std::vector<std::string>& key);

	// methods working with keys as escaped strings
	enum XConfigValueType get_type(const std::string& key) const;
	enum XConfigValueType get_type(const std::string& key);
	struct timespec get_mtime(const std::string& key) const;
	struct timespec get_mtime(const std::string& key);
	bool is_scalar(const std::string& key) const;
	bool is_scalar(const std::string& key);
	bool is_map(const std::string& key) const;
	bool is_map(const std::string& key);
	bool is_sequence(const std::string& key) const;
	bool is_sequence(const std::string& key);
	std::string get_string(const std::string& key) const;
	std::string get_string(const std::string& key);
	bool get_bool(const std::string& key) const;
	bool get_bool(const std::string& key);
	int get_int(const std::string& key) const;
	int get_int(const std::string& key);
	double get_float(const std::string& key) const;
	double get_float(const std::string& key);
	int get_count(const std::string& key) const;
	int get_count(const std::string& key);
	std::vector<std::string> get_map_keys(const std::string& key) const;
	std::vector<std::string> get_map_keys(const std::string& key);

	// methods working with node objects
	// configuration updates invalidate node objects
	enum XConfigValueType get_type(const XConfigNode& key) const;
	struct timespec get_mtime(const XConfigNode& key) const;
	bool is_scalar(const XConfigNode& key) const;
	bool is_map(const XConfigNode& key) const;
	bool is_sequence(const XConfigNode& key) const;
	std::string get_string(const XConfigNode& key) const;
	bool get_bool(const XConfigNode& key) const;
	int get_int(const XConfigNode& key) const;
	double get_float(const XConfigNode& key) const;
	int get_count(const XConfigNode& key) const;
	std::vector<std::string> get_map_keys(const XConfigNode& key) const;
	std::string get_name(const XConfigNode& key) const;

	// methods for transforming keys into nodes
	XConfigNode get_node(const std::string& key) const;
	XConfigNode get_node(const std::string& key);
	XConfigNode get_node(const std::vector<std::string>& key) const;
	XConfigNode get_node(const std::vector<std::string>& key);
	XConfigNode get_node_no_throw(const std::string& key) const;
	XConfigNode get_node_no_throw(const std::vector<std::string>& key) const;
	std::string get_key(const XConfigNode& node) const;

	// methods for tree iteration
	XConfigNode get_parent(const XConfigNode& key) const;
	std::vector<XConfigNode> get_children(const XConfigNode& key) const;

	// methods for escaping keys
	static std::string escape_key(const std::vector<std::string>& key);
	static std::string escape_key(const std::string& key);

	bool reload();

private:
	const boost::shared_ptr<XConfigConnection> conn;
	const void* hash;
	const XConfigBucket* buckets;
	const char* string_pool;
	const bool auto_reload;

	const XConfigBucket* get_bucket(const XConfigNode& key) const;
	std::string get_string(uint32_t offset) const;
	void do_reload();
};

class XConfigNode {
	friend class XConfig;
private:
	// bucket_idx == 0 => root node for parent, none for next field.
	// bucket_idx == 1 => first bucket
	uint32_t bucket_idx;
	// private so only available from XConfig
	XConfigNode(uint32_t b) : bucket_idx(b) { }
	operator uint32_t() const { return bucket_idx; }
public:
	XConfigNode() : bucket_idx(0) { }
};

} // namespace xconfig

#include "xconfig-inl.h"

#endif // LIBXCONFIG_XCONFIG_H_
