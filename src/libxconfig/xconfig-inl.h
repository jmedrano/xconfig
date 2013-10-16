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
inline int64_t XConfig::getInt(const std::vector<std::string>& key) const {
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
inline int64_t XConfig::getInt(const std::vector<std::string>& key) {
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
	try {
		return getString(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}
inline bool XConfig::getBool(const std::string& key) const {
	try {
		return getBool(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}
inline int64_t XConfig::getInt(const std::string& key) const {
	try {
		return getInt(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}
inline double XConfig::getFloat(const std::string& key) const {
	try {
		return getFloat(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}
inline int XConfig::getCount(const std::string& key) const {
	try {
		return getCount(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}
inline std::vector<std::string> XConfig::getMapKeys(const std::string& key) const {
	try {
		return getMapKeys(getNode(key));
	} catch (XConfigException &e) {
		e.setKey(key);
		throw e;
	}
}


inline enum XConfigValueType XConfig::getType(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getType(key);
}
inline struct timespec XConfig::getMtime(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getMtime(key);
}
inline bool XConfig::isScalar(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->isScalar(key);
}
inline bool XConfig::isMap(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->isMap(key);
}
inline bool XConfig::isSequence(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->isSequence(key);
}
inline std::string XConfig::getString(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getString(key);
}
inline bool XConfig::getBool(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getBool(key);
}
inline int64_t XConfig::getInt(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getInt(key);
}
inline double XConfig::getFloat(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getFloat(key);
}
inline int XConfig::getCount(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getCount(key);
}
inline std::vector<std::string> XConfig::getMapKeys(const std::string& key) {
	mightReload();
	return const_cast<const XConfig*>(this)->getMapKeys(key);
}

/**
 * Check if there is any update from the connection
 */
inline void XConfig::mightReload() {
	if (autoReload && conn->connect())
		applyReload();
}

inline enum XConfigValueType XConfigNode::getType() const {
	return xc->getType(*this);
}
inline struct timespec XConfigNode::getMtime() const {
	return xc->getMtime(*this);
}
inline bool XConfigNode::isScalar() const {
	return xc->isScalar(*this);
}
inline bool XConfigNode::isMap() const {
	return xc->isMap(*this);
}
inline bool XConfigNode::isSequence() const {
	return xc->isSequence(*this);
}
inline std::string XConfigNode::getString() const {
	return xc->getString(*this);
}
inline bool XConfigNode::getBool() const {
	return xc->getBool(*this);
}
inline int64_t XConfigNode::getInt() const {
	return xc->getInt(*this);
}
inline double XConfigNode::getFloat() const {
	return xc->getFloat(*this);
}
inline int XConfigNode::getCount() const {
	return xc->getCount(*this);
}
inline std::vector<std::string> XConfigNode::getMapKeys() const {
	return xc->getMapKeys(*this);
}
inline std::string XConfigNode::getName() const {
	return xc->getName(*this);
}
inline XConfigNode XConfigNode::getParent() const {
	return xc->getParent(*this);
}
inline std::vector<XConfigNode> XConfigNode::getChildren() const {
	return xc->getChildren(*this);
}

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_INL_H_
