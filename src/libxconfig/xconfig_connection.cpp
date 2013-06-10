#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <unistd.h>
#include <boost/thread.hpp>
#include <boost/make_shared.hpp>

#include <string>
#include <set>

#include "xconfig_connection.h"
#include "xconfig.h"

using std::string;
using boost::shared_ptr;
using boost::weak_ptr;
using boost::make_shared;
using boost::lock_guard;
using boost::mutex;

namespace xconfig {

const char* UnixConnection::WATCH_MSG = "watch";
const char* UnixConnection::PUSH_MSG = "push";
const char* UnixConnection::SEPARATOR = " ";
const char* UnixConnection::TERMINATOR = "\r\n";
const char* UnixConnection::DEFAULT_SOCKET = "/var/run/xconfig.socket";

void MappedFile::reset(int fd) {
	if (blob)
		munmap(blob, size);
	if (fd >= 0) {
		struct stat st;
		fstat(fd, &st);
		size = st.st_size;
		blob = mmap(0, size, PROT_READ, MAP_SHARED, fd, 0);
	} else {
		size = 0;
		blob = 0;
	}
}

bool LinkedConnection::connect() {
	auto new_map = conn->get_shared_map();
	bool ret = new_map != map;
	if (ret)
		map = std::move(new_map);
	return ret;
}

void LinkedConnection::close() {
	map.reset();
}

UnixConnection::UnixConnection(std::string path, std::string socket) : path(path),
		socket(socket.empty() ? DEFAULT_SOCKET: socket),
		socket_fd(-1)
{
}

UnixConnection::~UnixConnection()
{
	close();
}

bool UnixConnection::connect()
{
	if (socket_fd < 0) {
		// connect
		struct sockaddr_un addr;
		memset(&addr, 0, sizeof(addr));
		addr.sun_family = AF_UNIX;
		strncpy(addr.sun_path, socket.c_str(), sizeof(addr.sun_path)-1);

		socket_fd = ::socket(AF_UNIX, SOCK_STREAM, 0);
		if (socket_fd < 0)
			throw XConfigNotConnected();
		if (::connect(socket_fd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) < 0)
			throw XConfigNotConnected();
	}
	if (!map) {
		// send watch msg
		string msg = string(WATCH_MSG) + SEPARATOR + path + TERMINATOR;
		int written = ::write(socket_fd, msg.c_str(), msg.length());
		if (written != static_cast<int>(msg.length()))
			throw XConfigNotConnected();
	}
	if (socket_fd) {
		// look for push msg
		char data[1024], control[1024];
		struct msghdr msg;
		struct cmsghdr *cmsg;
		struct iovec iov;
		int tree_fd = -1;

		// process all pending messages
		for (;;) {
			memset(&msg, 0, sizeof(msg));
			iov.iov_base = data;
			iov.iov_len = sizeof(data)-1;
			msg.msg_iov = &iov;
			msg.msg_iovlen = 1;
			msg.msg_control = control;
			msg.msg_controllen = sizeof(control);

			// don't wait for push msg if a tree is already available
			int msg_flags = map || tree_fd >= 0 ? MSG_CMSG_CLOEXEC | MSG_DONTWAIT : MSG_CMSG_CLOEXEC;
			if (::recvmsg(socket_fd, &msg, msg_flags) < 0)
				break;

			data[iov.iov_len] = '\0';
			// loop over all control msgs
			cmsg = CMSG_FIRSTHDR(&msg);
			while (cmsg) {
				if (cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS) {
					if (tree_fd >= 0) {
						// more than one fd in msg. close every fd but the last one
						::close(tree_fd);
					}
					tree_fd = *reinterpret_cast<int *>(CMSG_DATA(cmsg));
				}
				cmsg = CMSG_NXTHDR(&msg, cmsg);
			}
		}
		if (tree_fd >= 0) {
			// tree received. let's mmap it
			atomic_store(&map, shared_ptr<const MappedFile>(new MappedFile(tree_fd)));
			::close(tree_fd);
			return true;
		}
	}
	return false;
}

void UnixConnection::close()
{
	if (socket_fd >= 0)
		::close(socket_fd);
	socket_fd = -1;
	atomic_store(&map, shared_ptr<const MappedFile>());
}

shared_ptr<const MappedFile> UnixConnection::get_shared_map() const
{
	return atomic_load(&map);
}

int UnixConnection::get_socket_fd() const
{
	return socket_fd;
}

shared_ptr<const MappedFile> FileConnection::get_shared_map() const
{
	return map;
}

UnixConnectionPool::UnixConnectionPool(bool local_thread_cache, int timeout)
		: shared_data(new SharedData), local_thread_cache(local_thread_cache) {
	shared_data->timeout = timeout;
	shared_data->epoll_fd = epoll_create(1);
	boost::thread thr(event_loop, weak_ptr<SharedData>(shared_data));
}
UnixConnectionPool::~UnixConnectionPool() {
}

boost::shared_ptr<LinkedConnection> UnixConnectionPool::get_connection(const std::string& path, std::string socket) {
	boost::shared_ptr<LinkedConnection> ret;
	if (socket.empty())
		socket = UnixConnection::DEFAULT_SOCKET;
	if (local_thread_cache) {
		auto& it = get_thread_local_map()[KeyType(path, socket)];
		if (!it) {
			it.reset(new LinkedConnection(get_shared_connection(path, socket)));
		}
		ret = it;
	} else {
		ret.reset(new LinkedConnection(get_shared_connection(path, socket)));
	}
	return ret;
}

void UnixConnectionPool::flush_local() {
	get_thread_local_map().clear();
}

boost::unordered_map<UnixConnectionPool::KeyType, UnixConnectionPool::LocalValueType>& UnixConnectionPool::get_thread_local_map() {
	if (!thread_local_map.get())
		thread_local_map.reset(new boost::unordered_map<KeyType, LocalValueType>());
	return *thread_local_map;
}

shared_ptr<UnixConnectionPool::LingerProxy> UnixConnectionPool::get_shared_connection(std::string path, std::string socket)
{
	KeyType key(path, socket);
	SharedData::HashMap::accessor accessor;
	bool inserted = shared_data->hash_map.insert(accessor, key);
	if (inserted) {
		// insert into hash_map
		accessor->second = make_shared<MapEntry>(key, shared_data->linger_list.end());
		UnixConnection& conn = accessor->second->unix_conn;

		// connect
		conn.connect();

		// insert into fd_map
		int socket_fd = conn.get_socket_fd();
		// NOTE: hash_map and fd_map are both locked
		bool inserted_in_fd_map = shared_data->fd_map.insert(std::pair<const int, KeyType>(socket_fd, key));
		assert(inserted_in_fd_map);

		// add socket to epoll
		struct epoll_event event;
		memset(&event, 0, sizeof(event));
		event.data.fd = socket_fd;
		event.events = EPOLLIN;
		int ctl_result = epoll_ctl(shared_data->epoll_fd, EPOLL_CTL_ADD, socket_fd, &event);
		assert(ctl_result >= 0);
	}

	auto linger_proxy = accessor->second->shared_conn.lock();
	if (!linger_proxy) {
		linger_proxy = make_shared<LingerProxy>(key, shared_data);
	}
	if (accessor->second->linger != shared_data->linger_list.end()) {
		// NOTE: hash_map and linger_list are both locked
		lock_guard<mutex> lock(shared_data->linger_list_mutex);
		shared_data->linger_list.erase(accessor->second->linger);
	}
	return linger_proxy;
}

void UnixConnectionPool::event_loop(const weak_ptr<SharedData>& shared_data) {
	// launch event loop on a new thread
	const int max_events = 10;
	const int epoll_timeout = 10000;
	struct epoll_event events[max_events];
	for (;;) {
		auto locked_data = shared_data.lock();
		if (!locked_data)
			break;
		int num_events = epoll_wait(locked_data->epoll_fd, events, max_events, epoll_timeout);
		for (int i = 0; i < num_events; i++) {
			int fd = events[i].data.fd;
			bool error = (events[i].events & EPOLLERR) || (events[i].events & EPOLLHUP);
			locked_data->on_read_event(fd, error);
		}
		locked_data->check_linger_list();
	}
}

void UnixConnectionPool::SharedData::on_read_event(int fd, bool error) {
	SharedData::FdMap::accessor fd_accessor;
	bool found = fd_map.find(fd_accessor, fd);
	if (!found || error) {
		int ctl_result = epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, 0);
		assert(ctl_result >= 0);
		if (!found)
			return;
		fd_map.erase(fd_accessor);
	}

	KeyType key(fd_accessor->second);
	fd_accessor.release();

	SharedData::HashMap::accessor accessor;
	found = hash_map.find(accessor, key);
	assert(found);
	UnixConnection& conn = accessor->second->unix_conn;
	accessor.release();

	// done without locks. no risk since deletes and calls to this method
	// are only done from the event loop thread
	if (error)
		conn.close();
	conn.connect();

	if (error) {
		int socket_fd = conn.get_socket_fd();
		bool inserted_in_fd_map = fd_map.insert(std::pair<const int, KeyType>(socket_fd, key));
		assert(inserted_in_fd_map);
	}
}

void UnixConnectionPool::SharedData::check_linger_list() {
	const int threshold = ::time(NULL) - timeout;
	// temporary set of items to be deleted after linger_list_mutex is released
	// this is done to avoid a deadlock having both linger_list_mutex and the hash_map accesor locked
	std::set<KeyType> keys_to_delete;
	{
		lock_guard<mutex> lock(linger_list_mutex);
		auto it = linger_list.begin();
		for (; it != linger_list.end(); ++it) {
			// list is sorted in ascendant time order so the rest of the items cannot be expired
			if (it->first > threshold)
				break;
			const KeyType& key = it->second;
			keys_to_delete.insert(key);
		}
		linger_list.erase(linger_list.begin(), it);
	}
	for (auto it = keys_to_delete.begin(); it != keys_to_delete.end(); ++it) {
		SharedData::HashMap::accessor accessor;
		bool found = hash_map.find(accessor, *it);
		assert(found);
		// we need to check if the entry has a new usage
		if (found && accessor->second->shared_conn.expired()) {
			int fd = accessor->second->unix_conn.get_socket_fd();
			int ctl_result = epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, 0);
			assert(ctl_result >= 0);
			fd_map.erase(fd);
			hash_map.erase(accessor);
		}
	}
}

UnixConnectionPool::LingerProxy::LingerProxy(const KeyType& key, const weak_ptr<SharedData>& shared_data)
	: key(key), shared_data(shared_data) {
}

UnixConnectionPool::LingerProxy::~LingerProxy() {
	shared_ptr<SharedData> locked_data(shared_data.lock());
	if (locked_data) {
		int timestamp = ::time(NULL);
		lock_guard<mutex> lock(locked_data->linger_list_mutex);
		locked_data->linger_list.push_back(std::pair<int, KeyType>(timestamp, key));
	}
}

bool UnixConnectionPool::LingerProxy::connect() {
	boost::shared_ptr<SharedData> locked_data(shared_data.lock());
	SharedData::HashMap::accessor accessor;
	bool found = locked_data->hash_map.find(accessor, key);
	assert(found);
	XConfigConnection& conn = accessor->second->unix_conn;
	return conn.connect();
}
void UnixConnectionPool::LingerProxy::close() {
	boost::shared_ptr<SharedData> locked_data(shared_data.lock());
	SharedData::HashMap::accessor accessor;
	bool found = locked_data->hash_map.find(accessor, key);
	assert(found);
	XConfigConnection& conn = accessor->second->unix_conn;
	return conn.close();
}
boost::shared_ptr<const MappedFile> UnixConnectionPool::LingerProxy::get_shared_map() const {
	boost::shared_ptr<SharedData> locked_data(shared_data.lock());
	SharedData::HashMap::accessor accessor;
	bool found = locked_data->hash_map.find(accessor, key);
	assert(found);
	XConfigConnection& conn = accessor->second->unix_conn;
	return conn.get_shared_map();
}

FileConnection::FileConnection(std::string path) : path(path)
{
}

FileConnection::~FileConnection()
{
	close();
}

bool FileConnection::connect()
{
	// don't reload
	if (map)
		return false;
	int fd = open(path.c_str(), O_RDONLY);
	if (fd < 0)
		return false;
	map.reset(new MappedFile(fd));
	::close(fd);
	return true;
}

void FileConnection::close()
{
	map.reset();
}

} // namespace xconfig
