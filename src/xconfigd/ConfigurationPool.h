#ifndef CONFIGURATION_POOL_H_
#define CONFIGURATION_POOL_H_

#include <string>
#include <QString>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <tbb/concurrent_hash_map.h>

#include "TLogger.h"

class ConfigurationTreeManager;

class ConfigurationPool {
	T_LOGGER_DECLARE(ConfigurationPool);
public:
	static ConfigurationPool& getInstance() {
		if (!instance) {
			instance = new ConfigurationPool;
		}
		return *instance;
	}

	boost::shared_ptr<ConfigurationTreeManager> getConfigurationManager(QString path);

private:
	static void deleteLaterConfigurationTreeManager(ConfigurationTreeManager* tree);

	tbb::concurrent_hash_map<std::string, boost::weak_ptr<ConfigurationTreeManager>> map;

	static ConfigurationPool* instance;
};

#endif
