#ifndef LIBXCONFIG_XCONFIG_CONNECTION_H_
#define LIBXCONFIG_XCONFIG_CONNECTION_H_

#include <string>
#include <boost/noncopyable.hpp>

namespace xconfig {

class MappedFile : private boost::noncopyable {
public:
	MappedFile() : size(0), blob(0) {}
	explicit MappedFile(int fd) : size(0), blob(0) {
		reset(fd);
	}
	~MappedFile() {
		reset();
	}
	void reset(int fd = -1);
	const void* get_blob() const {
		return blob;
	}
	operator bool() const {
		return blob;
	}
private:
	size_t size;
	void* blob;
};

class XConfigConnection : private boost::noncopyable {
public:
	virtual ~XConfigConnection() {}
	virtual bool connect() = 0;
	virtual void close() = 0;
	virtual const void* get_blob() const = 0;
};

class XConfigUnixConnection : public XConfigConnection {
public:
	static const char* WATCH_MSG;
	static const char* PUSH_MSG;
	static const char* SEPARATOR;
	static const char* TERMINATOR;
	static const char* DEFAULT_SOCKET;

	XConfigUnixConnection(std::string path, std::string socket = "");
	virtual ~XConfigUnixConnection();
	virtual bool connect();
	virtual void close();
	virtual const void* get_blob() const;
private:
	std::string path;
	std::string socket;
	int socket_fd;
	MappedFile map;
};

class XConfigFileConnection : public XConfigConnection {
public:
	explicit XConfigFileConnection(std::string path);
	virtual ~XConfigFileConnection();
	virtual bool connect();
	virtual void close();
	virtual const void* get_blob() const;
private:
	std::string path;
	MappedFile map;
};

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_CONNECTION_H_
