#ifndef XCONFIGDAEMON_H
#define XCONFIGDAEMON_H

#include "TApplication.h"
#include "TLogger.h"

#include <QDir>

class ServerSocket;

class XConfigDaemon : public TApplication
{
	Q_OBJECT
	Q_DISABLE_COPY(XConfigDaemon)
	T_LOGGER_DECLARE(XConfigDaemon);

public:
	XConfigDaemon(int& argc, char** argv);
	virtual ~XConfigDaemon();

	static XConfigDaemon* instance() { return qobject_cast<XConfigDaemon*>(QCoreApplication::instance()); }

	int getSoftTimeoutMsecs() const {
		return softTimeoutMsecs;
	}
	int getHardTimeoutMsecs() const {
		return hardTimeoutMsecs;
	}
	int getLingerTimeoutMsecs() const {
		return lingerTimeoutMsecs;
	}

protected:
	virtual bool init();
	virtual int run();

	virtual void signal_received(int signo);

	bool reloadConfig();

protected slots:
	void new_connection();

private:
	QDir config_dir;
	QString server_path;
	ServerSocket* server;
	int softTimeoutMsecs;
	int hardTimeoutMsecs;
	int lingerTimeoutMsecs;
};

#endif // XCONFIGDAEMON_H
