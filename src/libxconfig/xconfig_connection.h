#ifndef _XCONFIG_CONNECTION_H_
#define _XCONFIG_CONNECTION_H_

#include <string>

namespace xconfig {

class XConfigConnection {
public:
	virtual ~XConfigConnection() {}
	virtual bool connect() = 0;
	virtual void close() = 0;
	virtual const void* get_blob() = 0;
};

class XConfigUnixConnection : public XConfigConnection {
public:
	XConfigUnixConnection(std::string path, std::string socket = "");
	virtual ~XConfigUnixConnection();
	virtual bool connect();
	virtual void close();
	virtual const void* get_blob();
private:
	std::string path;
	std::string socket;
	int fd;
	size_t size;
	void* blob;
};

class XConfigFileConnection : public XConfigConnection {
public:
	XConfigFileConnection(std::string path);
	virtual ~XConfigFileConnection();
	virtual bool connect();
	virtual void close();
	virtual const void* get_blob();
private:
	std::string path;
	size_t size;
	void* blob;
};

} // namespace xconfig

#endif // _XCONFIG_CONNECTION_H_
