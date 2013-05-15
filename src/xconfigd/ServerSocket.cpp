#include "ServerSocket.h"
#include "FileLock.h"

ServerSocket::ServerSocket(const QString& name, QObject *parent) : QLocalServer(parent), socket_name(name), socket_lock(0)
{
}

ServerSocket::~ServerSocket()
{
	socket_lock->unlock();
}

bool ServerSocket::start()
{
	socket_lock = new FileLock(socket_name + ".lock");
	if (!socket_lock->tryLock())
		return false;
	QLocalServer::removeServer(socket_name);
	return listen(socket_name);
}

bool ServerSocket::stop()
{
	return true;
}

