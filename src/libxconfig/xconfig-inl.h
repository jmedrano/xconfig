#ifndef LIBXCONFIG_XCONFIG_INL_H_
#define LIBXCONFIG_XCONFIG_INL_H_

#include <time.h>
#include <string>
#include <vector>

namespace xconfig {

inline bool XConfig::isScalar(const XConfigNode& key) const {
	return xconfig::isScalar(getType(key));
}
inline bool XConfig::isMap(const XConfigNode& key) const {
	return getType(key) == TYPE_MAP;
}
inline bool XConfig::isSequence(const XConfigNode& key) const {
	return getType(key) == TYPE_SEQUENCE;
}

inline enum XConfigValueType XConfig::getType(const std::vector<std::string>& key) const {
	return getType(escapeKey(key));
}
inline struct timespec XConfig::getMtime(const std::vector<std::string>& key) const {
	return getMtime(escapeKey(key));
}
inline bool XConfig::isScalar(const std::vector<std::string>& key) const {
	return isScalar(escapeKey(key));
}
inline bool XConfig::isMap(const std::vector<std::string>& key) const {
	return isMap(escapeKey(key));
}
inline bool XConfig::isSequence(const std::vector<std::string>& key) const {
	return isSequence(escapeKey(key));
}
inline std::string XConfig::getString(const std::vector<std::string>& key) const {
	return getString(escapeKey(key));
}
inline bool XConfig::getBool(const std::vector<std::string>& key) const {
	return getBool(escapeKey(key));
}
inline int XConfig::getInt(const std::vector<std::string>& key) const {
	return getInt(escapeKey(key));
}
inline double XConfig::getFloat(const std::vector<std::string>& key) const {
	return getFloat(escapeKey(key));
}
inline int XConfig::getCount(const std::vector<std::string>& key) const {
	return getCount(escapeKey(key));
}
inline std::vector<std::string> XConfig::getMapKeys(const std::vector<std::string>& key) const {
	return getMapKeys(escapeKey(key));
}

inline enum XConfigValueType XConfig::getType(const std::vector<std::string>& key) {
	return getType(escapeKey(key));
}
inline struct timespec XConfig::getMtime(const std::vector<std::string>& key) {
	return getMtime(escapeKey(key));
}
inline bool XConfig::isScalar(const std::vector<std::string>& key) {
	return isScalar(escapeKey(key));
}
inline bool XConfig::isMap(const std::vector<std::string>& key) {
	return isMap(escapeKey(key));
}
inline bool XConfig::isSequence(const std::vector<std::string>& key) {
	return isSequence(escapeKey(key));
}
inline std::string XConfig::getString(const std::vector<std::string>& key) {
	return getString(escapeKey(key));
}
inline bool XConfig::getBool(const std::vector<std::string>& key) {
	return getBool(escapeKey(key));
}
inline int XConfig::getInt(const std::vector<std::string>& key) {
	return getInt(escapeKey(key));
}
inline double XConfig::getFloat(const std::vector<std::string>& key) {
	return getFloat(escapeKey(key));
}
inline int XConfig::getCount(const std::vector<std::string>& key) {
	return getCount(escapeKey(key));
}
inline std::vector<std::string> XConfig::getMapKeys(const std::vector<std::string>& key) {
	return getMapKeys(escapeKey(key));
}

inline enum XConfigValueType XConfig::getType(const std::string& key) const {
	return getType(getNode(key));
}
inline struct timespec XConfig::getMtime(const std::string& key) const {
	return getMtime(getNode(key));
}
inline bool XConfig::isScalar(const std::string& key) const {
	return isScalar(getNode(key));
}
inline bool XConfig::isMap(const std::string& key) const {
	return isMap(getNode(key));
}
inline bool XConfig::isSequence(const std::string& key) const {
	return isSequence(getNode(key));
}
inline std::string XConfig::getString(const std::string& key) const {
	return getString(getNode(key));
}
inline bool XConfig::getBool(const std::string& key) const {
	return getBool(getNode(key));
}
inline int XConfig::getInt(const std::string& key) const {
	return getInt(getNode(key));
}
inline double XConfig::getFloat(const std::string& key) const {
	return getFloat(getNode(key));
}
inline int XConfig::getCount(const std::string& key) const {
	return getCount(getNode(key));
}
inline std::vector<std::string> XConfig::getMapKeys(const std::string& key) const {
	return getMapKeys(getNode(key));
}


inline enum XConfigValueType XConfig::getType(const std::string& key) {
	mightReload();
	return getType(key);
}
inline struct timespec XConfig::getMtime(const std::string& key) {
	mightReload();
	return getMtime(key);
}
inline bool XConfig::isScalar(const std::string& key) {
	mightReload();
	return isScalar(key);
}
inline bool XConfig::isMap(const std::string& key) {
	mightReload();
	return isMap(key);
}
inline bool XConfig::isSequence(const std::string& key) {
	mightReload();
	return isSequence(key);
}
inline std::string XConfig::getString(const std::string& key) {
	mightReload();
	return getString(key);
}
inline bool XConfig::getBool(const std::string& key) {
	mightReload();
	return getBool(key);
}
inline int XConfig::getInt(const std::string& key) {
	mightReload();
	return getInt(key);
}
inline double XConfig::getFloat(const std::string& key) {
	mightReload();
	return getFloat(key);
}
inline int XConfig::getCount(const std::string& key) {
	mightReload();
	return getCount(key);
}
inline std::vector<std::string> XConfig::getMapKeys(const std::string& key) {
	mightReload();
	return getMapKeys(key);
}

/**
 * Check if there is any update from the connection
 * returns if an update is detected
 */
inline bool XConfig::mightReload() {
	if (autoReload && conn->connect()) {
		reload();
		return true;
	}
	return false;
}

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_INL_H_