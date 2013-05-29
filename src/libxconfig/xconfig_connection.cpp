#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <unistd.h>

#include <string>

#include "xconfig_connection.h"
#include "xconfig.h"

using std::string;

namespace xconfig {

const char* XConfigUnixConnection::WATCH_MSG = "watch";
const char* XConfigUnixConnection::PUSH_MSG = "push";
const char* XConfigUnixConnection::SEPARATOR = " ";
const char* XConfigUnixConnection::TERMINATOR = "\r\n";
const char* XConfigUnixConnection::DEFAULT_SOCKET = "/var/run/xconfig.socket";

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

XConfigUnixConnection::XConfigUnixConnection(std::string path, std::string socket) : path(path),
		socket(socket.empty() ? DEFAULT_SOCKET: socket),
		socket_fd(-1)
{
}

XConfigUnixConnection::~XConfigUnixConnection()
{
	close();
}

bool XConfigUnixConnection::connect()
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
			map.reset(tree_fd);
			::close(tree_fd);
			return true;
		}
	}
	return false;
}

void XConfigUnixConnection::close()
{
	if (socket_fd >= 0)
		::close(socket_fd);
	map.reset();
}

const void* XConfigUnixConnection::get_blob() const
{
	return map.get_blob();
}

XConfigFileConnection::XConfigFileConnection(std::string path) : path(path)
{
}

XConfigFileConnection::~XConfigFileConnection()
{
	close();
}

bool XConfigFileConnection::connect()
{
	// don't reload
	if (map)
		return false;
	int fd = open(path.c_str(), O_RDONLY);
	if (fd < 0)
		return false;
	map.reset(fd);
	::close(fd);
	return true;
}

void XConfigFileConnection::close()
{
	map.reset();
}

const void* XConfigFileConnection::get_blob() const
{
	return map.get_blob();
}

} // namespace xconfig
