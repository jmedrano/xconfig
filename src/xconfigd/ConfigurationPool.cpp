#include "ConfigurationPool.h"
#include "ConfigurationTree.h"
#include "XConfigDaemon.h"

T_LOGGER_DEFINE(ConfigurationPool, "ConfigurationPool");

ConfigurationPool* ConfigurationPool::instance = NULL;

void ConfigurationPool::deleteLaterConfigurationTreeManager(ConfigurationTreeManager* tree)
{
	auto path = tree->getPath();
	TTRACE("deleteLaterConfigurationTreeManager: %s", qPrintable(path));
	getInstance().map.erase(path.toStdString());
	tree->deleteLater();
}

boost::shared_ptr<ConfigurationTreeManager> ConfigurationPool::getConfigurationManager(QString path)
{
	boost::shared_ptr<ConfigurationTreeManager> ret;
	tbb::concurrent_hash_map<std::string, boost::weak_ptr<ConfigurationTreeManager>>::accessor accessor;
	if (!map.insert(accessor, path.toStdString())) {
		ret = accessor->second.lock();
	}
	auto daemon = XConfigDaemon::instance();
	int softTimeoutMsecs = daemon->getSoftTimeoutMsecs();
	int hardTimeoutMsecs = daemon->getHardTimeoutMsecs();
	int lingerTimeoutMsecs = daemon->getLingerTimeoutMsecs();
	if (!ret) {
		ret = boost::shared_ptr<ConfigurationTreeManager>(
			new ConfigurationTreeManager(path, softTimeoutMsecs, hardTimeoutMsecs, lingerTimeoutMsecs),
			deleteLaterConfigurationTreeManager);
		accessor->second = ret;
	}
	return ret;
}

