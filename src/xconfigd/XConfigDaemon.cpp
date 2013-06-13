#include "XConfigDaemon.h"
#include "ServerSocket.h"
#include "ConnectionManager.h"

#include <cmdline.hpp>
#include <help.hpp>

#include <QSettings>
#include <QLocalSocket>

#include <signal.h>

#include "config.h"

const char* LOG4CXX_CONFIG_FILE = "log4cxx.xml";
const char* XCONFIGD_CONFIG_FILE = "xconfigd.ini";

T_QLOGGER_DEFINE_ROOT(XConfigDaemon);

XConfigDaemon::XConfigDaemon(int& argc, char** argv) : TApplication(argc, argv), config_dir(QDir(PKGSYSCONFDIR)), server(0)
{
}

XConfigDaemon::~XConfigDaemon()
{
}

bool XConfigDaemon::init()
{
	TLogger::configure();
	this->setApplicationName("xconfigd");

	try {
		QtArgCmdLine cmdline;

		QtArg config('c', "config", "Configuration directory override", false, true);
		QtArg daemonize('d', "daemonize", "Daemonize on start", false, false);
		QtArg user('u', "user", "Run as the given user", false, true);

		QtArgHelp help(&cmdline);
		help.printer()->setProgramDescription("XConfig daemon.");
		help.printer()->setExecutableName(this->applicationName());

		cmdline.addArg(&config);
		cmdline.addArg(&daemonize);
		cmdline.addArg(&user);
		cmdline.addArg(help);
		cmdline.parse();

		if (config.isPresent())
			config_dir = QDir(config.value().toString());

		if (!config_dir.exists()) {
			TERROR("Invalid configuration directory: '%s'", qPrintable(config_dir.path()));
			return false;
		}

		config_dir.makeAbsolute();

		if (!TApplication::init(QList<int>() << SIGINT << SIGTERM << SIGHUP, daemonize.isPresent(), user.value().toString(), "xconfigd"))
			return false;
	} catch (const QtArgHelpHasPrintedEx& ex) {
		return false;
	} catch (const QtArgBaseException& ex) {
		TERROR("%s", ex.what());
		return false;
	}

	if (!reloadConfig())
		return false;

	server = new ServerSocket(server_path, this);
	connect(server, SIGNAL(newConnection()), SLOT(new_connection()));
	if (!server->start())
		return false;

	return true;
}

int XConfigDaemon::run()
{
	int res = TApplication::run();
	server->stop();
	return res;
}

void XConfigDaemon::signal_received(int signo)
{
	if (signo == SIGTERM || signo == SIGINT)
		quit();
	else if (signo == SIGHUP)
		reloadConfig();
}

bool XConfigDaemon::reloadConfig()
{
	TDEBUG("Load configuration...");

	if (!config_dir.exists(LOG4CXX_CONFIG_FILE)) {
		TWARN("Cannot find '%s'", LOG4CXX_CONFIG_FILE);
		return false;
	}

	if (!config_dir.exists(XCONFIGD_CONFIG_FILE)) {
		TWARN("Cannot find '%s'", XCONFIGD_CONFIG_FILE);
		return false;
	}

	TLogger::configure(qPrintable(config_dir.absoluteFilePath(LOG4CXX_CONFIG_FILE)));

	QSettings settings(config_dir.absoluteFilePath(XCONFIGD_CONFIG_FILE), QSettings::IniFormat);

	QString new_server_path = settings.value("ServerSocketPath", QString(PKGLOCALSTATEDIR "/server")).toString();
	if (server_path.isEmpty()) {
		server_path = new_server_path;
	} else if (server_path != new_server_path) {
		TWARN("Cannot change the server socket once the daemon is running");
	}

	return true;
}

const char* TLoggerRoot()
{
	return "xconfigd";
}

void XConfigDaemon::new_connection()
{
	new ConnectionManager(server->nextPendingConnection(), this);
}

int main(int argc, char **argv)
{
	XConfigDaemon app(argc, argv);
	return app.start();
}

