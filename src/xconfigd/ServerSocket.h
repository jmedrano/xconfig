#ifndef SERVERSOCKET_H
#define SERVERSOCKET_H

#include <QLocalServer>

class FileLock;

class ServerSocket : public QLocalServer {
	Q_OBJECT
public:
	
	ServerSocket(const QString& name, QObject *parent = 0);
	~ServerSocket();

	bool start();
	bool stop();

private:
	QString socket_name;
	FileLock* socket_lock;
};

#endif // SERVERSOCKET_H
