#include "ConfigurationTree.h"
#include "ConfigurationMerger.h"
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

void ConfigurationTreeManager::openTree() {
try {
printf("openTree\n");

	loadAllFiles();
printf("after loadAllFiles\n");

} catch (const std::exception& e) {
	abort();
}
}

ConfigurationTreeManager::ConfigurationTreeManager(QString path) {
printf("ConfigurationTreeManager\n");

	iNotifyFd = inotify_init();
	paths = path.split(':');
	auto base = paths.takeFirst().split(';');
	paths = base + paths;
	firstOverride = base.count();
	for (auto it = paths.begin(); it != paths.end(); ++it) {
		int watcher = inotify_add_watch(iNotifyFd, path.toLatin1().data(), IN_MODIFY);
		iWatchers.insert(watcher, path.toLatin1());
	}
	iNotifier = new QSocketNotifier(iNotifyFd, QSocketNotifier::Read, this);
	connect(iNotifier, SIGNAL(activated(int)), SLOT(onINotify()));

	QtConcurrent::run(this, &ConfigurationTreeManager::openTree);
}

ConfigurationTreeManager::~ConfigurationTreeManager() {
	printf("~ConfigurationTreeManager\n");
	iNotifier->deleteLater();
	// TODO move to when deleteLater is done
	::close(iNotifyFd);
	for (auto it = files.begin(); it != files.end(); ++it) {
		delete *it;
	}
}

void ConfigurationTreeManager::onINotify() {
	printf("onINotify\n");
	struct inotify_event event;
	event.len = 0;
	int nread = read(iNotifyFd, iNotifyBuffer, sizeof(iNotifyBuffer));
	for (size_t pos = 0; nread - pos > sizeof(event); pos += sizeof(event) + event.len) {
		memcpy(&event, iNotifyBuffer + pos, sizeof(event));
		QByteArray path = iWatchers[event.wd];

		assert(path.length());
		assert(event.mask == IN_MODIFY);

		printf("read from inotify %d %s\n", event.mask, path.data());
		if (event.len) {
			printf("event.name %s\n", iNotifyBuffer + sizeof(event));
		}

		QtConcurrent::run(this, &ConfigurationTreeManager::openTree);
	}
}

void ConfigurationTreeManager::loadAllFiles() {

	struct timespec a,b;
printf("loadAllFiles start\n");
clock_gettime(CLOCK_MONOTONIC, &a);

	baseFiles.clear();
	overrideFiles.clear();

	int numPaths = 0;
	std::set<std::string> fileNamesInDir;
	for (auto it = paths.begin(); it != paths.end(); ++it, ++numPaths) {
		QDir dir(*it, "*.yaml", QDir::NoSort, QDir::Files | QDir::Readable);
		auto filesInDir = dir.entryInfoList();
		for (auto f = filesInDir.begin(); f != filesInDir.end(); ++f) {
			std::string fileName = f->absoluteFilePath().toStdString();
			fileNamesInDir.insert(fileName);
			auto file = files.find(fileName);
			if (file == files.end()) {
				file = files.insert(fileName, new YamlParser(fileName));
			}
			(*file)->parse();
			if (numPaths < firstOverride) {
printf("loadAllFiles base: %s\n", fileName.c_str());
				baseFiles << *file;
			} else {
printf("loadAllFiles override: %s\n", fileName.c_str());
				overrideFiles << *file;
			}
		}

	}
	// Check for removed files
	if (size_t(files.count()) > fileNamesInDir.size()) {
printf("Checking for removed files\n");
		for (auto f = files.begin(); f != files.end();) {
			if (fileNamesInDir.find(f.key()) == fileNamesInDir.end()) {
printf("removed file: %s\n", f.key().c_str());
				delete *f;
				f = files.erase(f);
			} else {
printf("not removed file: %s\n", f.key().c_str());
				++f;
			}
		}
	}


clock_gettime(CLOCK_MONOTONIC, &b);
printf("lapsed %ld\n", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);
fflush(stdout);

printf("merger start\n");
	ConfigurationMerger merger(baseFiles, overrideFiles);
	merger.merge();

clock_gettime(CLOCK_MONOTONIC, &b);
printf("lapsed %ld\n", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);
printf("merge end\n");

	auto mergeResult = merger.dump();

clock_gettime(CLOCK_MONOTONIC, &b);
printf("lapsed %ld\n", (b.tv_sec - a.tv_sec) * 1000000 + (b.tv_nsec - a.tv_nsec) / 1000);
printf("loadAllFiles end\n");

	boost::atomic_store(&tree, boost::make_shared<const ConfigurationTree>(QString(mergeResult.first.c_str()), mergeResult.second));

	emit newTreeAvailable();
}

