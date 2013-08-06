#ifndef CONFIGURATION_TREE_H_
#define CONFIGURATION_TREE_H_

#include <boost/shared_ptr.hpp>

#include <QObject>
#include <QSocketNotifier>
#include <QByteArray>
#include <QMap>
#include <QStringList>

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

	void openTree();

Q_SIGNALS:
	void newTreeAvailable();

private Q_SLOTS:
	void onINotify();

private:
	void loadAllFiles();

	QStringList paths;
	QList<YamlParser*> baseFiles;
	QList<YamlParser*> overrideFiles;
	int firstOverride;
	boost::shared_ptr<const ConfigurationTree> tree;
	int iNotifyFd;
	QSocketNotifier* iNotifier;
	QMap<int, QByteArray> iWatchers;
	QMap<std::string, YamlParser*> files;
	char iNotifyBuffer[1024];
};

#endif
