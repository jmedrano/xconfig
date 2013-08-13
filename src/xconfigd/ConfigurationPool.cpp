#include "ConfigurationPool.h"
#include "ConfigurationTree.h"
#include "XConfigDaemon.h"

ConfigurationPool::ConfigurationPool* ConfigurationPool::instance = NULL;

static void deleteLaterConfigurationTreeManager(ConfigurationTreeManager* tree)
{
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

