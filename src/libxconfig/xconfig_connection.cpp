#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#include <string>

#include "xconfig_connection.h"

namespace xconfig {

XConfigUnixConnection::XConfigUnixConnection(std::string path, std::string socket) : path(path), socket(socket), blob(0)
{
}

XConfigUnixConnection::~XConfigUnixConnection()
{
	close();
}

bool XConfigUnixConnection::connect()
{
	// TODO
	return false;
}

void XConfigUnixConnection::close()
{
	if (fd > 0)
		::close(fd);
	if (blob)
		munmap(blob, size);
	blob = 0;
	size = 0;
}

const void* XConfigUnixConnection::get_blob()
{
	return blob;
}


XConfigFileConnection::XConfigFileConnection(std::string path) : path(path), blob(0)
{
}

XConfigFileConnection::~XConfigFileConnection()
{
	close();
}

bool XConfigFileConnection::connect()
{
	// don't reload
	if (blob)
		return false;
	int fd = open(path.c_str(), O_RDONLY);
	if (fd < 0)
		return false;
	struct stat st;
	fstat(fd, &st);
	size = st.st_size;
	blob = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
	::close(fd);
	return true;
}

void XConfigFileConnection::close()
{
	if (blob)
		munmap(blob, size);
	blob = 0;
	size = 0;
}

const void* XConfigFileConnection::get_blob()
{
	return blob;
}

} // namespace xconfig
