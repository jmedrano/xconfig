#ifndef SERVERSOCKET_H
#define SERVERSOCKET_H

#include "TLogger.h"

#include <QLocalSocket>
#include <QQueue>

class FileLock;
class QSocketNotifier;

class ServerSocket : public QObject {
	Q_OBJECT
	T_LOGGER_DECLARE(ConfigurationTreeManager);

Q_SIGNALS:
    void newConnection();

public:
	
	ServerSocket(const QString& name, QObject *parent = 0);
	~ServerSocket();

	int nextPendingConnection();
	bool start();
	bool stop();

private:
	QString socket_name;
	FileLock* socket_lock;
	int listenSocket;
	QSocketNotifier *socketNotifier;
	QQueue<int> pendingConnections;

	Q_DISABLE_COPY(ServerSocket)

private slots:
	void onNewConnection();
};

#endif // SERVERSOCKET_H
