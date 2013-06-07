#ifndef LIBXCONFIG_XCONFIG_CONNECTION_H_
#define LIBXCONFIG_XCONFIG_CONNECTION_H_

#include <string>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/tss.hpp>
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
	virtual boost::shared_ptr<const MappedFile> get_shared_map() const = 0;
	const void* get_blob() const {
		return get_shared_map()->get_blob();
	}
};

class LinkedConnection : public XConfigConnection {
public:
	explicit LinkedConnection(boost::shared_ptr<const XConfigConnection> conn) : conn(conn) {}
	virtual ~LinkedConnection() {}
	virtual bool connect();
	virtual void close();
	virtual boost::shared_ptr<const MappedFile> get_shared_map() const {
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
	boost::shared_ptr<const MappedFile> get_shared_map() const;
	int get_socket_fd() const;
private:
	const std::string path;
	const std::string socket;
	int socket_fd;
	boost::shared_ptr<const MappedFile> map;
};

class UnixConnectionPool {
public:
	typedef std::pair<std::string, std::string> KeyType;
	typedef boost::shared_ptr<LinkedConnection> LocalValueType;

	static const int DEFAULT_TIMEOUT = 30;

	explicit UnixConnectionPool(bool local_thread_cache = false, int timeout = DEFAULT_TIMEOUT);
	~UnixConnectionPool();
	boost::shared_ptr<LinkedConnection> get_connection(const std::string& path, std::string socket = "");
	void flush_local() {
		get_thread_local_map().clear();
	}

private:
	class LingerProxy;
	typedef std::list<std::pair<int, KeyType> > LingerList;
	struct MapEntry : private boost::noncopyable {
		UnixConnection unix_conn;
		boost::weak_ptr<LingerProxy> shared_conn;
		LingerList::iterator linger;

		MapEntry(const KeyType& key, const LingerList::iterator& linger)
			: unix_conn(key.first, key.second), linger(linger) {
		}
	};
	struct SharedData {
		typedef tbb::concurrent_hash_map<KeyType, boost::shared_ptr<MapEntry> > HashMap;
		typedef tbb::concurrent_hash_map<int, KeyType> FdMap;
		int timeout;
		HashMap hash_map;
		FdMap fd_map;
		LingerList linger_list;
		boost::mutex linger_list_mutex;
		int epoll_fd;

		void check_linger_list();
		void on_read_event(int fd, bool error);
	};
	class LingerProxy : public XConfigConnection {
	public:
		LingerProxy(const KeyType& key, const boost::weak_ptr<SharedData>& shared_data);
		~LingerProxy();
		virtual bool connect() {
			boost::shared_ptr<SharedData> locked_data(shared_data.lock());
			SharedData::HashMap::accessor accessor;
			bool found = locked_data->hash_map.find(accessor, key);
			assert(found);
			XConfigConnection& conn = accessor->second->unix_conn;
			return conn.connect();
		}
		virtual void close() {
			boost::shared_ptr<SharedData> locked_data(shared_data.lock());
			SharedData::HashMap::accessor accessor;
			bool found = locked_data->hash_map.find(accessor, key);
			assert(found);
			XConfigConnection& conn = accessor->second->unix_conn;
			return conn.close();
		}
		virtual boost::shared_ptr<const MappedFile> get_shared_map() const {
			boost::shared_ptr<SharedData> locked_data(shared_data.lock());
			SharedData::HashMap::accessor accessor;
			bool found = locked_data->hash_map.find(accessor, key);
			assert(found);
			XConfigConnection& conn = accessor->second->unix_conn;
			return conn.get_shared_map();
		}

	private:
		const KeyType key;
		const boost::weak_ptr<SharedData> shared_data;
	};

	boost::shared_ptr<LingerProxy> get_shared_connection(std::string path, std::string socket);
	boost::unordered_map<KeyType, LocalValueType>& get_thread_local_map() {
		if (!thread_local_map.get())
			thread_local_map.reset(new boost::unordered_map<KeyType, LocalValueType>());
		return *thread_local_map;
	}

	static void event_loop(const boost::weak_ptr<SharedData>& shared_data);

	boost::thread_specific_ptr<boost::unordered_map<KeyType, LocalValueType> > thread_local_map;
	boost::shared_ptr<SharedData> shared_data;
	bool local_thread_cache;
};

class FileConnection : public XConfigConnection {
public:
	explicit FileConnection(std::string path);
	virtual ~FileConnection();
	virtual bool connect();
	virtual void close();
	virtual boost::shared_ptr<const MappedFile> get_shared_map() const;
private:
	const std::string path;
	boost::shared_ptr<const MappedFile> map;
};

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_CONNECTION_H_
