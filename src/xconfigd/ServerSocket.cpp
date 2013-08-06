#include "ServerSocket.h"
#include "FileLock.h"

#include <qsocketnotifier.h>
#include <sys/socket.h>
#include <sys/un.h>


ServerSocket::ServerSocket(const QString& name, QObject *parent) : socket_name(name), socket_lock(0), socketNotifier(NULL)
{
}

ServerSocket::~ServerSocket()
{
	socket_lock->unlock();
	delete socketNotifier;
}

bool ServerSocket::start()
{
	socket_lock = new FileLock(socket_name + ".lock");
	if (!socket_lock->tryLock())
		return false;
	//QLocalServer::removeServer(socket_name);

	// TODO create socket
	listenSocket = ::socket(PF_UNIX, SOCK_SEQPACKET, 0);
	if (listenSocket < 0) {
		//setError(QLatin1String("QLocalServer::listen"));
		//closeServer();
		return false;
	}

	// Construct the unix address
	struct ::sockaddr_un addr;
	addr.sun_family = PF_UNIX;
	auto socket_name_latin1 = socket_name.toLatin1();
	if (sizeof(addr.sun_path) < (uint)socket_name_latin1.size() + 1) {
		//setError(QLatin1String("QLocalServer::listen"));
		//closeServer();
		return false;
	}
	::memcpy(addr.sun_path, socket_name_latin1.data(), socket_name_latin1.size() + 1);

	if (::bind(listenSocket, (sockaddr *)&addr, sizeof(sockaddr_un)) < 0) {
		if (errno == EADDRINUSE) {
			::close(listenSocket);
			listenSocket = -1;
		}
		return false;
	}

	if (::listen(listenSocket, 50) < 0) {
		return false;
	}

    socketNotifier = new QSocketNotifier(listenSocket, QSocketNotifier::Read, this);
    connect(socketNotifier, SIGNAL(activated(int)), this, SLOT(onNewConnection()));
	return true;
}

bool ServerSocket::stop()
{
	return true;
}

void ServerSocket::onNewConnection()
{
	if (-1 == listenSocket)
		return;

	::sockaddr_un addr;
	socklen_t length = sizeof(sockaddr_un);
	int connectedSocket = ::accept(listenSocket, (sockaddr *)&addr, &length);
	if (-1 == connectedSocket) {
		//setError(QLatin1String("QLocalSocket::activated"));
		//closeServer();
	} else {
		pendingConnections.enqueue(connectedSocket);
		emit newConnection();
	}
}

int ServerSocket::nextPendingConnection()
{
	if (pendingConnections.isEmpty())
		return 0;

	return pendingConnections.dequeue();
}

