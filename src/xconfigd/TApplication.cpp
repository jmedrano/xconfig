#include "TApplication.h"

#include <libdaemon/daemon.h>

#include <QSocketNotifier>

#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <unistd.h>

TApplication::TApplication(int& argc, char** argv) : QCoreApplication(argc, argv), daemon_signal_notifier(0), daemonized(false)
{
}

TApplication::~TApplication()
{
	if (daemon_signal_notifier) {
		daemon_signal_done();
		delete daemon_signal_notifier;
	}

	if (daemonized) {
		daemon_pid_file_remove();
	}
}

int TApplication::start()
{
	if (init())
		return run();
	return -1;
}

bool TApplication::init()
{
	return init(QList<int>());
}

bool TApplication::init(const QList<int>& hsignals, bool daemonize, const QString& user, const char* ident)
{
	pid_t process;
	if (daemonize) {
		daemon_pid_file_ident = ident;

		if ((process = daemon_pid_file_is_running()) > 0) {
			fprintf(stderr, "Process already running as PID: %d\n", process);
			return false;
		}

		struct passwd* user_pw = 0;
		if (!user.isEmpty()) {
			if (getuid() != 0) {
				fprintf(stderr, "You must be root to be able to change to another user\n");
				return false;
			}
			user_pw = getpwnam(user.toLatin1().data());
			if (!user_pw) {
				fprintf(stderr, "Unknown user '%s'\n", qPrintable(user));
				return false;
			}

			setgid(user_pw->pw_gid);
			setuid(user_pw->pw_uid);
		}

		process = daemon_fork();
		if (process < 0) {
			fprintf(stderr, "Error forking process\n");
			return false;
		} else if (process > 0) {
//			fprintf(stderr, "PID: %d\n", process);
			_exit(0);
		}

//		sleep(10);

		umask(0022);
		if (daemon_pid_file_create())
			fprintf(stderr, "Cannot create pid file (%s)\n", strerror(errno));

		daemonized = daemonize;
	}

	if (hsignals.size() == 0)
		daemon_signal_init(SIGINT, SIGTERM, 0);
	foreach (int signal, hsignals)
		daemon_signal_install(signal);
	daemon_signal_notifier = new QSocketNotifier(daemon_signal_fd(), QSocketNotifier::Read);
	connect(daemon_signal_notifier, SIGNAL(activated(int)), SLOT(daemon_signal_handler()));
	return true;
}

int TApplication::run()
{
	return exec();
}

void TApplication::signal_received(int signo)
{
	Q_UNUSED(signo);
	exit(1);
}

void TApplication::daemon_signal_handler()
{
	int signo;

	signo = daemon_signal_next();
	signal_received(signo);
}

