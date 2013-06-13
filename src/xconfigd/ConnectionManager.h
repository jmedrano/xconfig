#ifndef CONNECTIONMANAGER_H_
#define CONNECTIONMANAGER_H_

#include <QObject>
#include <QByteArray>
#include <QFlags>
#include <QPointer>
#include <QLocalSocket>

class QIODevice;

class ConnectionManager : public QObject {
	Q_OBJECT

public:
	enum State {
		STATE_INVALID = -1,
		STATE_IDLE = 0,
		STATE_RECEIVING_HANDSHAKE,
		STATE_RECEIVING_LINE
	};

	enum Capability {
		CAP_NONE = 0,
		CAP_FD_PASS = 1 << 0
	};
	Q_DECLARE_FLAGS(Capabilities, Capability);

public:
	ConnectionManager(QIODevice* connection, QObject* parent = 0);
	~ConnectionManager();

private:
	bool got_line(const char *line, size_t len);
	bool got_handshake(const char *handshake, size_t len);
	void abort();
	
private slots:
	void connection_ready_read();
	void connection_error(QLocalSocket::LocalSocketError error);

private:
	QPointer<QIODevice> connection;
	State state;
	QByteArray buffer;
	int buffer_pos;
	Capabilities capabilities;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(ConnectionManager::Capabilities);

#endif // CONNECTIONMANAGER_H_
