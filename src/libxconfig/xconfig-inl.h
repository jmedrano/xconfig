#ifndef LIBXCONFIG_XCONFIG_INL_H_
#define LIBXCONFIG_XCONFIG_INL_H_

#include <time.h>
#include <string>
#include <vector>

namespace xconfig {

inline bool XConfig::is_scalar(const XConfigNode& key) const {
	return xconfig::is_scalar(get_type(key));
}
inline bool XConfig::is_map(const XConfigNode& key) const {
	return get_type(key) == TYPE_MAP;
}
inline bool XConfig::is_sequence(const XConfigNode& key) const {
	return get_type(key) == TYPE_SEQUENCE;
}

inline enum XConfigValueType XConfig::get_type(const std::vector<std::string>& key) const {
	return get_type(escape_key(key));
}
inline struct timespec XConfig::get_mtime(const std::vector<std::string>& key) const {
	return get_mtime(escape_key(key));
}
inline bool XConfig::is_scalar(const std::vector<std::string>& key) const {
	return is_scalar(escape_key(key));
}
inline bool XConfig::is_map(const std::vector<std::string>& key) const {
	return is_map(escape_key(key));
}
inline bool XConfig::is_sequence(const std::vector<std::string>& key) const {
	return is_sequence(escape_key(key));
}
inline std::string XConfig::get_string(const std::vector<std::string>& key) const {
	return get_string(escape_key(key));
}
inline bool XConfig::get_bool(const std::vector<std::string>& key) const {
	return get_bool(escape_key(key));
}
inline int XConfig::get_int(const std::vector<std::string>& key) const {
	return get_int(escape_key(key));
}
inline double XConfig::get_float(const std::vector<std::string>& key) const {
	return get_float(escape_key(key));
}
inline int XConfig::get_count(const std::vector<std::string>& key) const {
	return get_count(escape_key(key));
}
inline std::vector<std::string> XConfig::get_map_keys(const std::vector<std::string>& key) const {
	return get_map_keys(escape_key(key));
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

inline enum XConfigValueType XConfig::get_type(const std::string& key) const {
	return get_type(get_node(key));
}
inline struct timespec XConfig::get_mtime(const std::string& key) const {
	return get_mtime(get_node(key));
}
inline bool XConfig::is_scalar(const std::string& key) const {
	return is_scalar(get_node(key));
}
inline bool XConfig::is_map(const std::string& key) const {
	return is_map(get_node(key));
}
inline bool XConfig::is_sequence(const std::string& key) const {
	return is_sequence(get_node(key));
}
inline std::string XConfig::get_string(const std::string& key) const {
	return get_string(get_node(key));
}
inline bool XConfig::get_bool(const std::string& key) const {
	return get_bool(get_node(key));
}
inline int XConfig::get_int(const std::string& key) const {
	return get_int(get_node(key));
}
inline double XConfig::get_float(const std::string& key) const {
	return get_float(get_node(key));
}
inline int XConfig::get_count(const std::string& key) const {
	return get_count(get_node(key));
}
inline std::vector<std::string> XConfig::get_map_keys(const std::string& key) const {
	return get_map_keys(get_node(key));
}


inline enum XConfigValueType XConfig::get_type(const std::string& key) {
	reload();
	return get_type(key);
}
inline struct timespec XConfig::get_mtime(const std::string& key) {
	reload();
	return get_mtime(key);
}
inline bool XConfig::is_scalar(const std::string& key) {
	reload();
	return is_scalar(key);
}
inline bool XConfig::is_map(const std::string& key) {
	reload();
	return is_map(key);
}
inline bool XConfig::is_sequence(const std::string& key) {
	reload();
	return is_sequence(key);
}
inline std::string XConfig::get_string(const std::string& key) {
	reload();
	return get_string(key);
}
inline bool XConfig::get_bool(const std::string& key) {
	reload();
	return get_bool(key);
}
inline int XConfig::get_int(const std::string& key) {
	reload();
	return get_int(key);
}
inline double XConfig::get_float(const std::string& key) {
	reload();
	return get_float(key);
}
inline int XConfig::get_count(const std::string& key) {
	reload();
	return get_count(key);
}
inline std::vector<std::string> XConfig::get_map_keys(const std::string& key) {
	reload();
	return get_map_keys(key);
}

class XConfigWrongType : public std::exception {
};

class XConfigNotFound : public std::exception {
};

class XConfigNotConnected : public std::exception {
};

/**
 * Check if there is any update from the connection
 * returns if an update is detected
 */
inline bool XConfig::reload() {
	if (auto_reload && conn->connect()) {
		do_reload();
		return true;
	}
	return false;
}

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_INL_H_
