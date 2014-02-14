#ifndef CONFIGURATION_TREE_H_
#define CONFIGURATION_TREE_H_

#include "TLogger.h"

#include <boost/shared_ptr.hpp>

#include <QObject>
#include <QSocketNotifier>
#include <QTimer>
#include <QByteArray>
#include <QMap>
#include <QSet>
#include <QStringList>
#include <QFuture>
#include <QMutex>

class YamlParser;

class ConfigurationTree {
public:
	ConfigurationTree(QString path, int fd) : path(path), fd(fd) {
	}
	~ConfigurationTree();

	QString path;
	int fd;
};

class ConfigurationTreeManager : public QObject {
	Q_OBJECT
	T_LOGGER_DECLARE(ConfigurationTreeManager);

public:
	ConfigurationTreeManager(QString path, int softTimeoutMsecs, int hardTimeoutMsecs, int lingerTimeoutMsecs);
	virtual ~ConfigurationTreeManager();

	QString getPath() {
		return path;
	}
	boost::shared_ptr<const ConfigurationTree> getConfigurationTree() {
		return boost::atomic_load(&tree);
	}
	void touch();

	void openTree();

Q_SIGNALS:
	void newTreeAvailable();

private Q_SLOTS:
	void onINotify();
	void onSoftCheck();
	void onHardCheck();
	void onLingerTimeout();

private:
	void loadAllFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder, bool somethingChanged = false);
	void loadFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder, QList<QString> files);
	void merge();

	QString path;
	int softTimeoutMsecs;
	int hardTimeoutMsecs;
	int lingerTimeoutMsecs;
	QStringList paths;
	QList<YamlParser*> baseFiles;
	QList<YamlParser*> overrideFiles;
	int firstOverride;
	boost::shared_ptr<const ConfigurationTree> tree;
	int iNotifyFd;
	QSocketNotifier* iNotifier;
	QTimer* hardTimer;
	QTimer* softTimer;
	QTimer* lingerTimer;
	boost::shared_ptr<ConfigurationTreeManager> lingerReference;
	QFuture<void> checkFuture;
	QMap<int, QByteArray> iWatchers;
	QMap<std::string, YamlParser*> filesMap;
	QSet<QString> dirtyFiles;
	QMutex mutex;
	char iNotifyBuffer[1024];
};

#endif
