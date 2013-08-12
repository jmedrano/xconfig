#include "ConfigurationTree.h"
#include "ConfigurationMerger.h"
#include "ConfigurationPool.h"
#include "YamlParser.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/inotify.h>
#include <fcntl.h>

#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

#include <QtConcurrentRun>
#include <QString>
#include <QStringList>
#include <QDir>

#include <set>


ConfigurationTree::~ConfigurationTree() {
	::close(fd);
	unlink(path.toLocal8Bit().data());
}

T_QLOGGER_DEFINE(ConfigurationTreeManager);

void ConfigurationTreeManager::openTree() {
	try {
		TTRACE("openTree");

		auto referenceHolder = ConfigurationPool::getInstance().getConfigurationManager(path);
		loadAllFiles(referenceHolder);
		TTRACE("after loadAllFiles");

	} catch (const std::exception& e) {
		abort();
	}
}

ConfigurationTreeManager::ConfigurationTreeManager(QString path) : path(path) {
	TTRACE("ConfigurationTreeManager");

	iNotifyFd = inotify_init();
	paths = path.split(':');
	auto base = paths.takeFirst().split(';');
	paths = base + paths;
	firstOverride = base.count();
	for (auto dirName = paths.begin(); dirName != paths.end(); ++dirName) {
		while (dirName->at(dirName->size()-1) == '/') {
			dirName->chop(1);
		}
		auto dirByteArray = dirName->toLocal8Bit();
		int watcher = inotify_add_watch(iNotifyFd, dirByteArray.data(), IN_MODIFY | IN_CREATE | IN_MOVED_FROM | IN_MOVED_TO | IN_DELETE);
		TTRACE("inotify_add_watch %s", dirByteArray.data());
		iWatchers.insert(watcher, dirByteArray);
	}
	iNotifier = new QSocketNotifier(iNotifyFd, QSocketNotifier::Read, this);
	connect(iNotifier, SIGNAL(activated(int)), SLOT(onINotify()));

	hardTimer = new QTimer(this);
	softTimer = new QTimer(this);
	lingerTimer = new QTimer(this);
	connect(hardTimer, SIGNAL(timeout()), SLOT(onHardCheck()));
	connect(softTimer, SIGNAL(timeout()), SLOT(onSoftCheck()));
	connect(lingerTimer, SIGNAL(timeout()), SLOT(onLingerTimeout()));
	// TODO get from config
	hardTimer->start(10000);

	QtConcurrent::run(this, &ConfigurationTreeManager::openTree);
}

ConfigurationTreeManager::~ConfigurationTreeManager() {
	TTRACE("~ConfigurationTreeManager");
	iNotifier->deleteLater();
	::close(iNotifyFd);
	for (auto it = filesMap.begin(); it != filesMap.end(); ++it) {
		delete *it;
	}
}

void ConfigurationTreeManager::onINotify() {
	TTRACE("onINotify");
	struct inotify_event event;
	event.len = 0;
	int nread = read(iNotifyFd, iNotifyBuffer, sizeof(iNotifyBuffer));
	bool somethingChanged = false;
	for (size_t pos = 0; nread - pos > sizeof(event); pos += sizeof(event) + event.len) {
		memcpy(&event, iNotifyBuffer + pos, sizeof(event));
		QByteArray dirName = iWatchers[event.wd];

		assert(dirName.length());

		TTRACE("read from inotify %d %s", event.mask, dirName.data());
		if (event.len) {
			TTRACE("event.name %s", iNotifyBuffer + sizeof(event));
			// filter out files with leading . (rsync)
			if (iNotifyBuffer[sizeof(event)] == '.') {
				// TODO filter out non .yaml files
				continue;
			}
			QString fileName = dirName + '/' + QString(&iNotifyBuffer[sizeof(event)]);
			TDEBUG("inotify: modified fileName=[%s]", fileName.toLatin1().data());
			dirtyFiles << fileName;
			somethingChanged = true;
		} else {
			// event on dir itself
			// adding dir to dirtyFiles will force a hard check
			dirtyFiles << dirName;
		}

	}
	if (somethingChanged) {
		// TODO get from config
		softTimer->start(100);
	}
}

void ConfigurationTreeManager::loadAllFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder) {
	Q_UNUSED(referenceHolder);
	QMutexLocker locker(&mutex);

	struct timespec a,b;
	TTRACE("loadAllFiles start");
	clock_gettime(CLOCK_MONOTONIC, &a);

	baseFiles.clear();
	overrideFiles.clear();

	bool somethingChanged = false;

	int numPaths = 0;
	std::set<std::string> fileNamesInDir;
	for (auto it = paths.begin(); it != paths.end(); ++it, ++numPaths) {
		QDir dir(*it, "*.yaml", QDir::NoSort, QDir::Files | QDir::Readable);
		auto filesInDir = dir.entryInfoList();
		for (auto f = filesInDir.begin(); f != filesInDir.end(); ++f) {
			std::string fileName = f->absoluteFilePath().toStdString();
			auto file = filesMap.find(fileName);
			if (file == filesMap.end()) {
				file = filesMap.insert(fileName, new YamlParser(fileName));
				somethingChanged = true;
			}
			try {
				somethingChanged |= (*file)->parse();
				if (numPaths < firstOverride) {
					TTRACE("loadAllFiles base: %s", fileName.c_str());
					baseFiles << *file;
				} else {
					TTRACE("loadAllFiles override: %s", fileName.c_str());
					overrideFiles << *file;
				}
				fileNamesInDir.insert(fileName);
			} catch (const YamlNotFoundException &e) {
				TDEBUG("deleted file %s", fileName.c_str());
				somethingChanged = true;
			}
		}

	}
	// Check for removed files
	if (size_t(filesMap.count()) > fileNamesInDir.size()) {
		TTRACE("Checking for removed files");
		for (auto f = filesMap.begin(); f != filesMap.end();) {
			if (fileNamesInDir.find(f.key()) == fileNamesInDir.end()) {
				TTRACE("removed file: %s", f.key().c_str());
				somethingChanged = true;
				delete *f;
				f = filesMap.erase(f);
			} else {
				TTRACE("not removed file: %s", f.key().c_str());
				++f;
			}
		}
	}


	clock_gettime(CLOCK_MONOTONIC, &b);
	TDEBUG("loadAllFiles took %ld usecs", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);

	if (somethingChanged) {
		TTRACE("something changed");
		merge();
	} else {
		TTRACE("nothing changed");
	}
}

void ConfigurationTreeManager::loadFiles(boost::shared_ptr<ConfigurationTreeManager> referenceHolder, QList<QString> files) {
	Q_UNUSED(referenceHolder);
	bool areDirsModified = false;
	// locked block
	{
		QMutexLocker locker(&mutex);

		bool areFilesModified = false;
		for (auto fileName = files.begin(); fileName != files.end(); ++fileName) {
				auto file = filesMap.find(fileName->toStdString());
				if (file == filesMap.end()) {
					TDEBUG("new file %s", fileName->toLatin1().data());
					areDirsModified = true;
					break;
					// TODO new file
				} else {
					// TODO could have been deleted
					try {
						bool isModified = (*file)->parse();
						TDEBUG("modified file %s = %d", fileName->toLatin1().data(), isModified);
						areFilesModified |= isModified;
					} catch (const YamlNotFoundException &e) {
						TDEBUG("deleted file %s", fileName->toLocal8Bit().data());
						areDirsModified = true;
					}
				}
		}
		if (areFilesModified && ! areDirsModified) {
			TTRACE("soft merge");
			merge();
			return;
		}
	}

	if (areDirsModified) {
		TTRACE("dirs modified");
		loadAllFiles(referenceHolder);
	} else {
		TTRACE("nothing changed");
	}
}

void ConfigurationTreeManager::merge() {
	struct timespec a,b;
	clock_gettime(CLOCK_MONOTONIC, &a);
	TTRACE("merger start");
	ConfigurationMerger merger(baseFiles, overrideFiles);
	merger.merge();

	clock_gettime(CLOCK_MONOTONIC, &b);
	TDEBUG("merge took %ld usecs", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);

	auto mergeResult = merger.dump();

	clock_gettime(CLOCK_MONOTONIC, &b);
	TDEBUG("dump took %ld usecs", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);

	boost::atomic_store(&tree, boost::make_shared<const ConfigurationTree>(QString(mergeResult.first.c_str()), mergeResult.second));
	emit newTreeAvailable();
}

void ConfigurationTreeManager::onHardCheck() {
	TTRACE("onHardCheck");
	if (!hardCheckFuture.isStarted() || hardCheckFuture.isFinished()) {
		auto referenceHolder = ConfigurationPool::getInstance().getConfigurationManager(path);
		hardCheckFuture = QtConcurrent::run(this, &ConfigurationTreeManager::loadAllFiles, referenceHolder);
	}
}

void ConfigurationTreeManager::onSoftCheck() {
	TTRACE("onSoftCheck");
	if (!softCheckFuture.isStarted() || softCheckFuture.isFinished()) {
		auto referenceHolder = ConfigurationPool::getInstance().getConfigurationManager(path);
		softCheckFuture = QtConcurrent::run(this, &ConfigurationTreeManager::loadFiles, referenceHolder, dirtyFiles.toList());
		dirtyFiles.clear();
		softTimer->stop();
	}
}

void ConfigurationTreeManager::onLingerTimeout() {
	TTRACE("onLingerTimeout");
	lingerReference.reset();
}

void ConfigurationTreeManager::touch() {
	if (!lingerReference)
		lingerReference = ConfigurationPool::getInstance().getConfigurationManager(path);
	// TODO get from config
	lingerTimer->start(20000);
}
