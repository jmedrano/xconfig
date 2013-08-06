#ifndef CONFIGURATION_POOL_H_
#define CONFIGURATION_POOL_H_

#include <string>
#include <QString>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>
#include <tbb/concurrent_hash_map.h>

class ConfigurationTreeManager;

class ConfigurationPool {
public:
	static ConfigurationPool& getInstance() {
		if (!instance) {
			instance = new ConfigurationPool;
		}
		return *instance;
	}

	boost::shared_ptr<ConfigurationTreeManager> getConfigurationManager(QString path);

	// TODO implement some method to cleanup empty entries

private:
	tbb::concurrent_hash_map<std::string, boost::weak_ptr<ConfigurationTreeManager>> map;

	static ConfigurationPool* instance;
};

#endif
