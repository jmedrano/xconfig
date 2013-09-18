#ifndef CONNECTIONMANAGER_H_
#define CONNECTIONMANAGER_H_

#include "TLogger.h"

#include <QObject>
#include <QByteArray>
#include <QFlags>
#include <QPointer>
#include <QLocalSocket>
#include <QSocketNotifier>

#include <boost/shared_ptr.hpp>

class ConfigurationTreeManager;

class ConnectionManager : public QObject {
	Q_OBJECT
	T_LOGGER_DECLARE(ConfigurationTreeManager);

public:
	enum Capability {
		CAP_NONE = 0,
		CAP_FD_PASS = 1 << 0
	};
	Q_DECLARE_FLAGS(Capabilities, Capability);

public:
	ConnectionManager(int connectionFd, QObject* parent = 0);
	~ConnectionManager();

private:
	void sendDatagram(const char* buf, size_t len, int fd = -1);
	void receiveDatagram(const char* buf, size_t len);
	void receiveWatch(const char* buf, size_t len);
	bool receiveHandshake(const char *buf, size_t len);
	void sendHi();
	void sendPush(QString path, int fd);
	
private slots:
	void connectionReadyRead();
	void connectionClose();
	void onNewTreeAvailable();

private:
	int connectionFd;
	boost::shared_ptr<ConfigurationTreeManager> treeManager;
	QSocketNotifier* notifier;
	QByteArray buffer;
	Capabilities capabilities;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(ConnectionManager::Capabilities);

#endif // CONNECTIONMANAGER_H_
