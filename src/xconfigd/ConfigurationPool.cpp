#include "ConfigurationPool.h"
#include "ConfigurationTree.h"

#include <boost/make_shared.hpp>

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
	if (!ret) {
		//ret = boost::make_shared<ConfigurationTreeManager>(path);
		ret = boost::shared_ptr<ConfigurationTreeManager>(new ConfigurationTreeManager(path), deleteLaterConfigurationTreeManager);
		accessor->second = ret;
	}
	return ret;
}

