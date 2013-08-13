#include "ServerSocket.h"
#include "FileLock.h"

#include <qsocketnotifier.h>
#include <QLocalServer>
#include <sys/socket.h>
#include <sys/un.h>

T_QLOGGER_DEFINE(ServerSocket);

ServerSocket::ServerSocket(const QString& name, QObject *parent) : QObject(parent), socket_name(name),
		socket_lock(0), listenSocket(-1), socketNotifier(NULL)
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
	QLocalServer::removeServer(socket_name);

	// create socket
	listenSocket = ::socket(PF_UNIX, SOCK_SEQPACKET, 0);
	if (listenSocket < 0) {
		TERROR("can't create socket: %s", strerror(errno));
		return false;
	}

	// Construct the unix address
	struct ::sockaddr_un addr;
	addr.sun_family = PF_UNIX;
	auto socket_name_latin1 = socket_name.toLatin1();
	if (sizeof(addr.sun_path) < (uint)socket_name_latin1.size() + 1) {
		TERROR("socket name [%s] is too big", socket_name_latin1.data());
		::close(listenSocket);
		listenSocket = -1;
		return false;
	}
	::memcpy(addr.sun_path, socket_name_latin1.data(), socket_name_latin1.size() + 1);

	if (::bind(listenSocket, (sockaddr *)&addr, sizeof(sockaddr_un)) < 0) {
		TERROR("can't bind on socket: %s", strerror(errno));
		::close(listenSocket);
		listenSocket = -1;
		return false;
	}

	if (::listen(listenSocket, 50) < 0) {
		TERROR("can't listen on socket: %s", strerror(errno));
		::close(listenSocket);
		listenSocket = -1;
		return false;
	}

    socketNotifier = new QSocketNotifier(listenSocket, QSocketNotifier::Read, this);
    connect(socketNotifier, SIGNAL(activated(int)), this, SLOT(onNewConnection()));

	TDEBUG("started new ServerSocket:%s", socket_name_latin1.data());

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
		char errstr[BUFSIZ];
		strerror_r(errno, errstr, sizeof(errstr));
		TERROR("error on accept %s", errstr);
	} else {
		TDEBUG("accepted new connection");
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

