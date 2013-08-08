#include "ConnectionManager.h"
#include "ConfigurationTree.h"
#include "ConfigurationPool.h"

#include <sys/types.h>
#include <sys/socket.h>

#include <boost/make_shared.hpp>

#define DECLARE_CAP(c) c, sizeof(c) - 1
static const int VERSION = 1;
static struct { const char *name; size_t len; ConnectionManager::Capability capability; } CAPABILITIES[] = {
	{ DECLARE_CAP("FD_PASS"), ConnectionManager::CAP_FD_PASS }
};
static const int MAX_DGRAM = 1024;
static const char* HANDSHAKE_EXPECTED = "CAPABILITIES (";
static const int HANDSHAKE_EXPECTED_LEN = strlen(HANDSHAKE_EXPECTED);

ConnectionManager::ConnectionManager(int connectionFd, QObject* parent) : QObject(parent), connectionFd(connectionFd)
{
	notifier = new QSocketNotifier(connectionFd, QSocketNotifier::Read, this);

	sendHi();

	connect(notifier, SIGNAL(activated(int)), SLOT(connectionReadyRead()));
	//connect(connection, SIGNAL(error(QLocalSocket::LocalSocketError)), SLOT(connectionError(QLocalSocket::LocalSocketError)));

	buffer.resize(MAX_DGRAM);
}

ConnectionManager::~ConnectionManager()
{
	if (treeManager) {
		treeManager->touch();
	}
	if (connectionFd > 0) {
		::close(connectionFd);
	}
}

void ConnectionManager::connectionReadyRead()
{
	struct msghdr msg;
	struct iovec iov;
	printf("ConnectionManager::connectionReadyRead\n");

	/* Response data */
	iov.iov_base = buffer.data();
	iov.iov_len = buffer.length();

	/* compose the message */
	memset(&msg, 0, sizeof(msg));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	while(connectionFd >= 0) {
		int nread = recvmsg(connectionFd, &msg, MSG_DONTWAIT);
		if (nread == 0) {
			abort();
			return;
		} else if (nread < 0) {
			if (errno != EAGAIN) {
				perror("recvmsg error");
				//close(connFd);
				abort();
			}
			return;
		}

		receiveDatagram(buffer.data(), nread);
	}

}

void ConnectionManager::connectionError(QLocalSocket::LocalSocketError error)
{
	Q_UNUSED(error);
	printf("error\n");
	abort();
}

bool ConnectionManager::receiveHandshake(const char *buf, size_t len)
{
	printf("got handshake: '%s'\n", buf);
	if (strncmp(buf, HANDSHAKE_EXPECTED, HANDSHAKE_EXPECTED_LEN) != 0)
		return false;
	const char *haystack = buf + strlen(HANDSHAKE_EXPECTED);
	const char *needle;
//	printf("%s\n", haystack);
	while ((needle = strpbrk(haystack, ",)")) != NULL) {
		bool found = false;
		if ((needle - haystack) == 0 && *needle == ')')
			break;
		for (unsigned int i = 0; i < (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])); i++) {
//			printf("%d %d\n", needle-haystack, CAPABILITIES[i].len);
			if ((size_t)(needle - haystack) == CAPABILITIES[i].len && strncmp(CAPABILITIES[i].name, haystack, qMin((size_t)(needle - haystack), strlen(CAPABILITIES[i].name))) == 0) {
				if (capabilities & CAPABILITIES[i].capability)
					return false;
				capabilities |= CAPABILITIES[i].capability;
				found = true;
//				printf("cap found\n");
				break;
			}
		}
		if (!found)
			return false;
		if (*needle == ')')
			break;
		haystack = needle + 1;
	}

//	printf("%d => %d\n", len, (size_t)(needle - buf + 1));
	if (!needle || *needle != ')' || (len != (size_t)(needle - buf + 1)))
		return false;
	return true;
}

void ConnectionManager::receiveWatch(const char *buf, size_t len)
{
	printf("got watch\n");

	if (treeManager) {
		disconnect(treeManager.get(), 0, 0, 0);
	}

	// TODO improve this
	QString path(QByteArray(buf + 6, len - 6 - 2));

printf("before getConfigurationManager\n");
	treeManager = ConfigurationPool::getInstance().getConfigurationManager(path);
printf("after getConfigurationManager\n");

	if (treeManager) {
		treeManager->touch();
		connect(treeManager.get(), SIGNAL(newTreeAvailable()), SLOT(onNewTreeAvailable()));
		onNewTreeAvailable();
	}
}

void ConnectionManager::onNewTreeAvailable()
{
printf("onNewTreeAvailable\n");
	assert(treeManager);
	auto tree = treeManager->getConfigurationTree();
	if (tree) {
printf("got path %s\n", tree->path.toLatin1().data());
		sendPush(tree->path, tree->fd);
	}
}

void ConnectionManager::abort()
{
	printf("abort\n");

	notifier->setEnabled(false);
	::close(connectionFd);
	connectionFd = -1;
	if (treeManager) {
		disconnect(treeManager.get(), 0, 0, 0);
printf("disconnected");
		treeManager.reset();
	}
	deleteLater();
}

void ConnectionManager::sendDatagram(const char* buf, size_t len, int controlFd)
{
	char control[sizeof(struct cmsghdr)+10];
	struct msghdr msg;
	struct cmsghdr *cmsg;
	struct iovec iov;

	/* Response data */
	iov.iov_base = const_cast<char*>(buf);
	iov.iov_len = len;

	/* compose the message */
	memset(&msg, 0, sizeof(msg));
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;

	/* send controlFd */
	if (controlFd > 0) {
		msg.msg_control = control;
		msg.msg_controllen = sizeof(control);
		cmsg = CMSG_FIRSTHDR(&msg);
		cmsg->cmsg_level = SOL_SOCKET;
		cmsg->cmsg_type = SCM_RIGHTS;
		cmsg->cmsg_len = CMSG_LEN(sizeof(controlFd));
		memcpy(reinterpret_cast<void *>(CMSG_DATA(cmsg)), &controlFd, sizeof(controlFd));
		msg.msg_controllen = cmsg->cmsg_len;
	}

	if (sendmsg(connectionFd, &msg, MSG_DONTWAIT) < 0) {
		perror("sendmsg error");
		abort();
		//close(connFd);
		return;
	}
}

void ConnectionManager::receiveDatagram(const char* buf, size_t len)
{
	if (memcmp(buf, "watch ", 6) == 0) {
		receiveWatch(buf, len);
	} else if (memcmp(buf, "CAPABILITIES ", 6) == 0) {
		receiveHandshake(buf, len);
	} else {
		printf("WAT\n");
	}
}

void ConnectionManager::sendHi()
{
	QByteArray handshake;

	handshake.append("XConfigD VERSION ");
	handshake.append(QString::number(VERSION));
	handshake.append(" CAPABILITIES (");
	for (unsigned int i = 0; i < (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])); i++) {
		handshake.append(CAPABILITIES[i].name);
		if (i != (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])) - 1)
			handshake.append(",");
	}
	handshake.append(")\n");

	sendDatagram(handshake.data(), handshake.length());
}

void ConnectionManager::sendPush(QString path, int fd)
{
	QByteArray push;
printf("send push\n");

	push.append("PUSH ");
	push.append(path);
	push.append("\n");

	sendDatagram(push.data(), push.length(), fd);
}
