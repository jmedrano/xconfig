#ifndef LIBXCONFIG_XCONFIG_CONNECTION_H_
#define LIBXCONFIG_XCONFIG_CONNECTION_H_

#include <string>
#include <list>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread.hpp>
#include <tbb/concurrent_hash_map.h>

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
	const void* getBlob() const {
		return blob;
	}
	operator bool() const {
		return blob;
	}

private:
	void reset(int fd = -1);

	size_t size;
	void* blob;
};

class XConfigConnection : private boost::noncopyable {
public:
	virtual ~XConfigConnection() {}
	virtual bool connect() = 0;
	virtual void close() = 0;
	virtual boost::shared_ptr<const MappedFile> getMap() const = 0;
};

class LinkedConnection : public XConfigConnection {
public:
	explicit LinkedConnection(boost::shared_ptr<const XConfigConnection> conn) : conn(conn) {}
	virtual ~LinkedConnection() {}
	virtual bool connect();
	virtual void close();
	virtual boost::shared_ptr<const MappedFile> getMap() const {
		return map;
	}
private:
	const boost::shared_ptr<const XConfigConnection> conn;
	boost::shared_ptr<const MappedFile> map;
};

class UnixConnection : public XConfigConnection {
public:
	static const char* WATCH_MSG;
	static const char* PUSH_MSG;
	static const char* SEPARATOR;
	static const char* TERMINATOR;
	static const char* DEFAULT_SOCKET;

	UnixConnection(std::string path, std::string socket = "");
	virtual ~UnixConnection();
	virtual bool connect();
	virtual void close();
	boost::shared_ptr<const MappedFile> getMap() const;
	int getSockedFd() const;
private:
	const std::string path;
	const std::string socket;
	int socketFd;
	boost::shared_ptr<const MappedFile> map;
};

class UnixConnectionPool {
public:
	typedef std::pair<std::string, std::string> KeyType;
	typedef boost::shared_ptr<LinkedConnection> LocalValueType;

	static const int DEFAULT_TIMEOUT = 30;

	// Callback reasons
	static const int CALLBACK_RELOAD = 1;
	static const int CALLBACK_CLOSE = 2;

	explicit UnixConnectionPool(int timeout = DEFAULT_TIMEOUT, bool localThreadCache = false);
	~UnixConnectionPool();

	// Deprecated: Use setCallbackInfo
	void setReloadCallback(void (*reloadCallback)(void*), void* reloadCallbackData);

	// Set a callback that receives information about why it's being called
	// int: Indicates the reason of the call
	// void*: We'll provide extra information depending on the reason
	// void*: User-supplied pointer
	void setCallbackInfo(void (*callbackInfo)(int, void*, void*), void* callbackData);
	boost::shared_ptr<LinkedConnection> getConnection(const std::string& path, std::string socket = "");
	void flushLocal();

private:
	class LingerProxy;
	typedef std::list<std::pair<int, KeyType> > LingerList;
	struct MapEntry : private boost::noncopyable {
		UnixConnection unixConn;
		boost::weak_ptr<LingerProxy> sharedConn;
		LingerList::iterator linger;

		MapEntry(const KeyType& key, const LingerList::iterator& linger)
			: unixConn(key.first, key.second), linger(linger) {
		}
	};
	struct SharedData {
		typedef tbb::concurrent_hash_map<KeyType, boost::shared_ptr<MapEntry> > HashMap;
		typedef tbb::concurrent_hash_map<int, KeyType> FdMap;
		int timeout;
		HashMap hashMap;
		FdMap fdMap;
		LingerList lingerList;
		boost::mutex lingerListMutex;
		int epollFd;
		int closingPipe[2];
		void (*reloadCallback)(void*);
		void (*callbackInfo)(int, void*, void*);
		void* callbackData;

		void checkLingerList();
		void onReadEvent(int fd, bool error);
	};
	class LingerProxy : public XConfigConnection {
	public:
		LingerProxy(const KeyType& key, const boost::weak_ptr<SharedData>& sharedData);
		~LingerProxy();
		virtual bool connect();
		virtual void close();
		virtual boost::shared_ptr<const MappedFile> getMap() const;

	private:
		const KeyType key;
		const boost::weak_ptr<SharedData> sharedData;
	};

	boost::unordered_map<KeyType, LocalValueType>& getThreadLocalMap();
	boost::shared_ptr<LingerProxy> getSharedConnection(std::string path, std::string socket);

	static void eventLoop(const boost::weak_ptr<SharedData>& sharedData);

	boost::thread_specific_ptr<boost::unordered_map<KeyType, LocalValueType> > threadLocalMap;
	boost::shared_ptr<SharedData> sharedData;
	bool localThreadCache;
	boost::thread eventLoopThread;
};

class FileConnection : public XConfigConnection {
public:
	explicit FileConnection(std::string path);
	virtual ~FileConnection();
	virtual bool connect();
	virtual void close();
	virtual boost::shared_ptr<const MappedFile> getMap() const;
private:
	const std::string path;
	boost::shared_ptr<const MappedFile> map;
};

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_CONNECTION_H_
