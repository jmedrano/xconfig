#ifndef _XCONFIG_H_
#define _XCONFIG_H_

#include <time.h>
#include <string>
#include <vector>
#include <boost/scoped_ptr.hpp>

#include "xconfig_file.h"
#include "xconfig_connection.h"

typedef std::string XConfigKeyType;

class XConfigNode;

class XConfig {
public:
	static const char map_separator = '/';
	static const char sequence_separator = '#';
	static const char escape_character = '\\';
	static const char* escaped_characters;
	static const XConfigNode null_node;

	XConfig();
	explicit XConfig(XConfigConnection* conn);
	~XConfig();
	void connect();
	void close();

	// methods working with keys as unescaped string vectors
	enum XConfigValueType get_type(const std::vector<std::string>& key);
	struct timespec get_mtime(const std::vector<std::string>& key);
	bool is_scalar(const std::vector<std::string>& key);
	bool is_map(const std::vector<std::string>& key);
	bool is_sequence(const std::vector<std::string>& key);
	std::string get_string(const std::vector<std::string>& key);
	bool get_bool(const std::vector<std::string>& key);
	int get_int(const std::vector<std::string>& key);
	double get_float(const std::vector<std::string>& key);
	int get_count(const std::vector<std::string>& key);
	std::vector<std::string> get_map_keys(const std::vector<std::string>& key);

	// methods working with keys as escaped strings
	enum XConfigValueType get_type(const XConfigKeyType& key);
	struct timespec get_mtime(const XConfigKeyType& key);
	bool is_scalar(const XConfigKeyType& key);
	bool is_map(const XConfigKeyType& key);
	bool is_sequence(const XConfigKeyType& key);
	std::string get_string(const XConfigKeyType& key);
	bool get_bool(const XConfigKeyType& key);
	int get_int(const XConfigKeyType& key);
	double get_float(const XConfigKeyType& key);
	int get_count(const XConfigKeyType& key);
	std::vector<std::string> get_map_keys(const XConfigKeyType& key);

	// methods working with node objects
	// configuration updates invalidate node objects
	enum XConfigValueType get_type(const XConfigNode& key);
	struct timespec get_mtime(const XConfigNode& key);
	bool is_scalar(const XConfigNode& key);
	bool is_map(const XConfigNode& key);
	bool is_sequence(const XConfigNode& key);
	std::string get_string(const XConfigNode& key);
	bool get_bool(const XConfigNode& key);
	int get_int(const XConfigNode& key);
	double get_float(const XConfigNode& key);
	int get_count(const XConfigNode& key);
	std::vector<std::string> get_map_keys(const XConfigNode& key);
	std::string get_name(const XConfigNode& key);

	// methods for transforming keys into nodes
	XConfigNode get_node(const XConfigKeyType& key);
	XConfigNode get_node(const std::vector<std::string>& key);
	XConfigNode get_node_no_throw(const XConfigKeyType& key);
	XConfigNode get_node_no_throw(const std::vector<std::string>& key);
	XConfigKeyType get_key(const XConfigNode& node);

	// methods for tree iteration
	XConfigNode get_parent(const XConfigNode& key);
	std::vector<XConfigNode> get_children(const XConfigNode& key);

	// methods for escaping keys
	static XConfigKeyType escape_key(const std::vector<std::string>& key);
	static std::string escape_key(const std::string& key);

private:
	boost::scoped_ptr<XConfigConnection> conn;
	const void* hash;
	const XConfigBucket* buckets;
	const char* string_pool;

	const XConfigBucket* get_bucket(const XConfigNode& key);
	std::string get_string(uint32_t offset);
	void check_connection();
	void update_connection();

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

inline bool XConfig::is_scalar(const XConfigNode& key) {
	return get_type(key) > XConfigTypeSequence;
}
inline bool XConfig::is_map(const XConfigNode& key) {
	return get_type(key) == XConfigTypeMap;
}
inline bool XConfig::is_sequence(const XConfigNode& key) {
	return get_type(key) == XConfigTypeSequence;
}

inline enum XConfigValueType XConfig::get_type(const std::vector<std::string>& key) {
	return get_type(escape_key(key));
}
inline struct timespec XConfig::get_mtime(const std::vector<std::string>& key) {
	return get_mtime(escape_key(key));
}
inline bool XConfig::is_scalar(const std::vector<std::string>& key) {
	return is_scalar(escape_key(key));
}
inline bool XConfig::is_map(const std::vector<std::string>& key) {
	return is_map(escape_key(key));
}
inline bool XConfig::is_sequence(const std::vector<std::string>& key) {
	return is_sequence(escape_key(key));
}
inline std::string XConfig::get_string(const std::vector<std::string>& key) {
	return get_string(escape_key(key));
}
inline bool XConfig::get_bool(const std::vector<std::string>& key) {
	return get_bool(escape_key(key));
}
inline int XConfig::get_int(const std::vector<std::string>& key) {
	return get_int(escape_key(key));
}
inline double XConfig::get_float(const std::vector<std::string>& key) {
	return get_float(escape_key(key));
}
inline int XConfig::get_count(const std::vector<std::string>& key) {
	return get_count(escape_key(key));
}
inline std::vector<std::string> XConfig::get_map_keys(const std::vector<std::string>& key) {
	return get_map_keys(escape_key(key));
}

inline enum XConfigValueType XConfig::get_type(const XConfigKeyType& key) {
	check_connection();
	return get_type(get_node(key));
}
inline struct timespec XConfig::get_mtime(const XConfigKeyType& key) {
	check_connection();
	return get_mtime(get_node(key));
}
inline bool XConfig::is_scalar(const XConfigKeyType& key) {
	check_connection();
	return is_scalar(get_node(key));
}
inline bool XConfig::is_map(const XConfigKeyType& key) {
	check_connection();
	return is_map(get_node(key));
}
inline bool XConfig::is_sequence(const XConfigKeyType& key) {
	check_connection();
	return is_sequence(get_node(key));
}
inline std::string XConfig::get_string(const XConfigKeyType& key) {
	check_connection();
	return get_string(get_node(key));
}
inline bool XConfig::get_bool(const XConfigKeyType& key) {
	check_connection();
	return get_bool(get_node(key));
}
inline int XConfig::get_int(const XConfigKeyType& key) {
	check_connection();
	return get_int(get_node(key));
}
inline double XConfig::get_float(const XConfigKeyType& key) {
	check_connection();
	return get_float(get_node(key));
}
inline int XConfig::get_count(const XConfigKeyType& key) {
	check_connection();
	return get_count(get_node(key));
}
inline std::vector<std::string> XConfig::get_map_keys(const XConfigKeyType& key) {
	check_connection();
	return get_map_keys(get_node(key));
}

class XConfigWrongType : public std::exception {
};

class XConfigNotFound : public std::exception {
};

class XConfigNotConnected : public std::exception {
};

/**
 * Check if there is any update from the connection
 * if there is no transaction in progress
 * If an update is detected
 */
inline void XConfig::check_connection() {
	if (conn->connect())
		update_connection();
}


#endif // _XCONFIG_H_
