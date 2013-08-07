#ifndef CONFIGURATION_TREE_H_
#define CONFIGURATION_TREE_H_

#include <boost/shared_ptr.hpp>

#include <QObject>
#include <QSocketNotifier>
#include <QTimer>
#include <QByteArray>
#include <QMap>
#include <QStringList>
#include <QFuture>
#include <QMutex>

class YamlParser;

class ConfigurationTree {
public:
	ConfigurationTree(QString path, int fd) : path(path), fd(fd) {
	}
	~ConfigurationTree() {
printf("~ConfigurationTree\n");
		::close(fd);
	}

	QString path;
	int fd;
};

class ConfigurationTreeManager : public QObject {
	Q_OBJECT

public:
	ConfigurationTreeManager(QString path);
	virtual ~ConfigurationTreeManager();

	boost::shared_ptr<const ConfigurationTree> getConfigurationTree() {
		return tree;
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
	void loadAllFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder);
	void loadFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder, QList<QString> files);
	void merge();

	QString path;
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
	QFuture<void> hardCheckFuture;
	QFuture<void> softCheckFuture;
	QMap<int, QByteArray> iWatchers;
	QMap<std::string, YamlParser*> filesMap;
	QList<QString> dirtyFiles;
	QMutex mutex;
	char iNotifyBuffer[1024];
};

#endif
