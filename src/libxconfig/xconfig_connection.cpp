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
#include "config.h"

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
const char* UnixConnection::DEFAULT_SOCKET = PKGLOCALSTATEDIR "/server";

void MappedFile::reset(int fd) {
	if (blob)
		munmap(blob, size);
	if (fd >= 0) {
		struct stat st;
		if (fstat(fd, &st) < 0) {
			perror("fstat");
			return;
		}
		void * mmaped = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
		if (mmaped == MAP_FAILED) {
			perror("mmap");
			return;
		}
		size = st.st_size;
		blob = mmaped;
	} else {
		size = 0;
		blob = 0;
	}
}

bool LinkedConnection::connect() {
	auto newMap = conn->getMap();
	bool ret = newMap && (newMap != map);
	if (ret)
		map = std::move(newMap);
	return ret;
}

void LinkedConnection::close() {
	map.reset();
}

UnixConnection::UnixConnection(std::string path, std::string socket) : path(path),
		socket(socket.empty() ? DEFAULT_SOCKET: socket),
		socketFd(-1)
{
}

UnixConnection::~UnixConnection()
{
	close();
}

bool UnixConnection::connect()
{
	if (socketFd < 0) {
		// connect
		struct sockaddr_un addr;
		memset(&addr, 0, sizeof(addr));
		addr.sun_family = AF_UNIX;
		strncpy(addr.sun_path, socket.c_str(), sizeof(addr.sun_path)-1);

		socketFd = ::socket(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0);
		if (socketFd < 0)
			throw XConfigNotConnected();
		if (::connect(socketFd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) < 0) {
			::close(socketFd);
			socketFd = -1;
			throw XConfigNotConnected();
		}
	}
	if (!map) {
		// send watch msg
		string msg = string(WATCH_MSG) + SEPARATOR + path + TERMINATOR;
		int written = ::write(socketFd, msg.c_str(), msg.length());
		if (written != static_cast<int>(msg.length()))
			throw XConfigNotConnected();
	}
	if (socketFd) {
		// look for push msg
		char data[1024], control[1024];
		struct msghdr msg;
		struct cmsghdr *cmsg;
		struct iovec iov;
		int treeFd = -1;

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
			int msg_flags = map || treeFd >= 0 ? MSG_CMSG_CLOEXEC | MSG_DONTWAIT : MSG_CMSG_CLOEXEC;
			int nread = ::recvmsg(socketFd, &msg, msg_flags);
			if (nread <= 0)
				break;

			data[iov.iov_len] = '\0';
			// loop over all control msgs
			cmsg = CMSG_FIRSTHDR(&msg);
			while (cmsg) {
				if (cmsg->cmsg_level == SOL_SOCKET && cmsg->cmsg_type == SCM_RIGHTS) {
					if (treeFd >= 0) {
						// more than one fd in msg. close every fd but the last one
						::close(treeFd);
					}
					memcpy(&treeFd, reinterpret_cast<void *>(CMSG_DATA(cmsg)), sizeof(treeFd));
				}
				cmsg = CMSG_NXTHDR(&msg, cmsg);
			}
		}
		if (treeFd >= 0) {
			// tree received. let's mmap it
			atomic_store(&map, shared_ptr<const MappedFile>(new MappedFile(treeFd)));
			::close(treeFd);
			return true;
		}
	}
	return false;
}

void UnixConnection::close()
{
	if (socketFd >= 0)
		::close(socketFd);
	socketFd = -1;
	atomic_store(&map, shared_ptr<const MappedFile>());
}

shared_ptr<const MappedFile> UnixConnection::getMap() const
{
	return atomic_load(&map);
}

int UnixConnection::getSockedFd() const
{
	return socketFd;
}

shared_ptr<const MappedFile> FileConnection::getMap() const
{
	return map;
}

UnixConnectionPool::UnixConnectionPool(int timeout, bool localThreadCache)
		: sharedData(new SharedData), localThreadCache(localThreadCache) {
	sharedData->timeout = timeout;
	sharedData->epollFd = epoll_create1(EPOLL_CLOEXEC);
	sharedData->reloadCallback = 0;
	if (pipe2(sharedData->closingPipe, O_CLOEXEC) < 0) {
		perror("pipe2");
		abort();
	}

	struct epoll_event event;
	memset(&event, 0, sizeof(event));
	event.data.fd = sharedData->closingPipe[0];
	event.events = EPOLLIN;
	int ctlResult = epoll_ctl(sharedData->epollFd, EPOLL_CTL_ADD, sharedData->closingPipe[0], &event);
	if (ctlResult < 0) {
		perror("epoll_ctl");
		abort();
	}

	eventLoopThread = boost::thread(eventLoop, weak_ptr<SharedData>(sharedData));
}
UnixConnectionPool::~UnixConnectionPool() {
	char c = 0;
	int flags = fcntl(sharedData->closingPipe[1], F_GETFL, 0);
	fcntl(sharedData->closingPipe[1], F_SETFL, flags | O_NONBLOCK);
	int nwritten = ::write(sharedData->closingPipe[1], &c, sizeof(c));
	if (nwritten < 1) {
		return;
	}
	eventLoopThread.timed_join(boost::posix_time::seconds(10));

	::close(sharedData->epollFd);
	::close(sharedData->closingPipe[0]);
	::close(sharedData->closingPipe[1]);
}

void UnixConnectionPool::setReloadCallback(void (*reloadCallback)(void*), void* reloadCallbackData) {
	sharedData->reloadCallback = reloadCallback;
	sharedData->reloadCallbackData = reloadCallbackData;
}

boost::shared_ptr<LinkedConnection> UnixConnectionPool::getConnection(const std::string& path, std::string socket) {
	boost::shared_ptr<LinkedConnection> ret;
	if (socket.empty())
		socket = UnixConnection::DEFAULT_SOCKET;
	if (localThreadCache) {
		auto& it = getThreadLocalMap()[KeyType(path, socket)];
		if (!it) {
			it.reset(new LinkedConnection(getSharedConnection(path, socket)));
		}
		ret = it;
	} else {
		ret.reset(new LinkedConnection(getSharedConnection(path, socket)));
	}
	return ret;
}

void UnixConnectionPool::flushLocal() {
	getThreadLocalMap().clear();
}

boost::unordered_map<UnixConnectionPool::KeyType, UnixConnectionPool::LocalValueType>& UnixConnectionPool::getThreadLocalMap() {
	if (!threadLocalMap.get())
		threadLocalMap.reset(new boost::unordered_map<KeyType, LocalValueType>());
	return *threadLocalMap;
}

shared_ptr<UnixConnectionPool::LingerProxy> UnixConnectionPool::getSharedConnection(std::string path, std::string socket)
{
	KeyType key(path, socket);
	SharedData::HashMap::accessor accessor;
	bool inserted = sharedData->hashMap.insert(accessor, key);
	if (inserted) {
		// insert into hashMap
		accessor->second = boost::make_shared<MapEntry>(key, sharedData->lingerList.end());
		UnixConnection& conn = accessor->second->unixConn;

		// connect
		try {
			conn.connect();
		} catch (const XConfigNotConnected& e) {
			// can't connect: erase from map and rethrow
			sharedData->hashMap.erase(accessor);
			throw e;
		}

		// insert into fdMap
		int socketFd = conn.getSockedFd();
		// NOTE: hashMap and fdMap are both locked
		bool insertedInFdMap = sharedData->fdMap.insert(std::pair<const int, KeyType>(socketFd, key));
		if (!insertedInFdMap)
			abort();

		// add socket to epoll
		struct epoll_event event;
		memset(&event, 0, sizeof(event));
		event.data.fd = socketFd;
		event.events = EPOLLIN;
		int ctlResult = epoll_ctl(sharedData->epollFd, EPOLL_CTL_ADD, socketFd, &event);
		if (ctlResult < 0) {
			perror("epoll_ctl");
			abort();
		}
	}

	auto lingerProxy = accessor->second->sharedConn.lock();
	if (!lingerProxy) {
		lingerProxy = boost::make_shared<LingerProxy>(key, sharedData);
		accessor->second->sharedConn = lingerProxy;
	}
	if (accessor->second->linger != sharedData->lingerList.end()) {
		// NOTE: hashMap and lingerList are both locked
		lock_guard<mutex> lock(sharedData->lingerListMutex);
		sharedData->lingerList.erase(accessor->second->linger);
		accessor->second->linger = sharedData->lingerList.end();
	}
	return lingerProxy;
}

void UnixConnectionPool::eventLoop(const weak_ptr<SharedData>& sharedData) {
	// launch event loop on a new thread
	const int maxEvents = 10;
	const int epollTimeout = 10000;
	struct epoll_event events[maxEvents];
	for (;;) {
		auto lockedData = sharedData.lock();
		if (!lockedData)
				break;
		int epollFd = lockedData->epollFd;
		lockedData.reset();
		int numEvents = epoll_wait(epollFd, events, maxEvents, epollTimeout);
		lockedData = sharedData.lock();
		if (!lockedData)
			break;
		for (int i = 0; i < numEvents; i++) {
			int fd = events[i].data.fd;
			bool error = (events[i].events & EPOLLERR) || (events[i].events & EPOLLHUP);
			if (fd == lockedData->closingPipe[0]) {
				return;
			}
			lockedData->onReadEvent(fd, error);
		}
		lockedData->checkLingerList();
	}
}

void UnixConnectionPool::SharedData::onReadEvent(int fd, bool error) {

	KeyType key;
	SharedData::FdMap::accessor fdAccessor;
	bool found = fdMap.find(fdAccessor, fd);
	if (found)
		key = fdAccessor->second;
	if (!found || error) {
		int ctlResult = epoll_ctl(epollFd, EPOLL_CTL_DEL, fd, 0);
		if (ctlResult < 0) {
			perror("epoll_ctl");
			abort();
		}
		if (!found)
			return;
		fdMap.erase(fdAccessor);
	}

	fdAccessor.release();

	SharedData::HashMap::accessor accessor;
	found = hashMap.find(accessor, key);
	if (!found)
		abort();
	UnixConnection& conn = accessor->second->unixConn;
	accessor.release();

	// done without locks. no risk since deletes and calls to this method
	// are only done from the event loop thread
	if (error)
		conn.close();
	else {
		conn.connect();
		if (reloadCallback)
			reloadCallback(reloadCallbackData);
	}
}

void UnixConnectionPool::SharedData::checkLingerList() {
	const int threshold = ::time(NULL) - timeout;
	// temporary set of items to be deleted after lingerListMutex is released
	// this is done to avoid a deadlock having both lingerListMutex and the hashMap accesor locked
	std::set<KeyType> keysToDelete;
	{
		lock_guard<mutex> lock(lingerListMutex);
		auto it = lingerList.begin();
		for (; it != lingerList.end(); ++it) {
			// list is sorted in ascendant time order so the rest of the items cannot be expired
			if (it->first > threshold)
				break;
			const KeyType& key = it->second;
			keysToDelete.insert(key);
		}
		lingerList.erase(lingerList.begin(), it);
	}
	for (auto it = keysToDelete.begin(); it != keysToDelete.end(); ++it) {
		SharedData::HashMap::accessor accessor;
		bool found = hashMap.find(accessor, *it);
		if (!found) {
			// already deleted
			abort();
		}
		// we need to check if the entry has a new usage
		if (found && accessor->second->sharedConn.expired()) {
			int fd = accessor->second->unixConn.getSockedFd();
			int ctlResult = epoll_ctl(epollFd, EPOLL_CTL_DEL, fd, 0);
			if (ctlResult < 0) {
				perror("epoll_ctl");
				abort();
			}
			fdMap.erase(fd);
			hashMap.erase(accessor);
		}
	}
}

UnixConnectionPool::LingerProxy::LingerProxy(const KeyType& key, const weak_ptr<SharedData>& sharedData)
	: key(key), sharedData(sharedData) {
}

UnixConnectionPool::LingerProxy::~LingerProxy() {
	shared_ptr<SharedData> lockedData(sharedData.lock());
	if (lockedData) {
		int timestamp = ::time(NULL);
		LingerList::iterator lingerEntry;

		// NOTE: hashMap and lingerList are both locked
		SharedData::HashMap::accessor accessor;
		bool found = lockedData->hashMap.find(accessor, key);
		lock_guard<mutex> lock(lockedData->lingerListMutex);

		lockedData->lingerList.push_back(std::pair<int, KeyType>(timestamp, key));
		lingerEntry = --lockedData->lingerList.end();
		if (found) {
			accessor->second->linger = lingerEntry;
		}
	}
}

bool UnixConnectionPool::LingerProxy::connect() {
	boost::shared_ptr<SharedData> lockedData(sharedData.lock());
	if (!lockedData)
		return false;
	SharedData::HashMap::accessor accessor;
	bool found = lockedData->hashMap.find(accessor, key);
	if (!found)
		abort();
	XConfigConnection& conn = accessor->second->unixConn;
	return conn.connect();
}
void UnixConnectionPool::LingerProxy::close() {
	boost::shared_ptr<SharedData> lockedData(sharedData.lock());
	SharedData::HashMap::accessor accessor;
	bool found = lockedData->hashMap.find(accessor, key);
	if (!found) {
		// why?
		fprintf(stderr, "LingerProxy::close(): Can't find key in hashmap");
		return;
	}
	XConfigConnection& conn = accessor->second->unixConn;
	return conn.close();
}
boost::shared_ptr<const MappedFile> UnixConnectionPool::LingerProxy::getMap() const {
	boost::shared_ptr<SharedData> lockedData(sharedData.lock());
	if (!lockedData)
		return boost::shared_ptr<const MappedFile>();
	SharedData::HashMap::accessor accessor;
	bool found = lockedData->hashMap.find(accessor, key);
	if (!found) {
		// why?
		fprintf(stderr, "LingerProxy::getMap(): Can't find key in hashmap");
		return boost::shared_ptr<const MappedFile>();
	}
	UnixConnection& conn = accessor->second->unixConn;
	if (conn.getSockedFd() < 0) {
		try {
			conn.connect();
		} catch (const XConfigNotConnected& e) {
			return boost::shared_ptr<const MappedFile>();
		}
		int newSocketFd = conn.getSockedFd();
		if (newSocketFd >= 0) {
			bool insertedInFdMap = lockedData->fdMap.insert(std::pair<const int, KeyType>(newSocketFd, key));
			if (!insertedInFdMap) {
				// why?
				fprintf(stderr, "LingerProxy::getMap(): Not connected and can't insert");
			}
			// add socket to epoll
			struct epoll_event event;
			memset(&event, 0, sizeof(event));
			event.data.fd = newSocketFd;
			event.events = EPOLLIN;
			int ctlResult = epoll_ctl(lockedData->epollFd, EPOLL_CTL_ADD, newSocketFd, &event);
			if (ctlResult < 0) {
				perror("epoll_ctl");
				abort();
			}
		}
	}
	return conn.getMap();
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
