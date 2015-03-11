#ifndef YAML_CONFIGURATION_MERGER_H
#define YAML_CONFIGURATION_MERGER_H

#include "YamlParser.h"
#include "TLogger.h"

#include <xconfig_file.h>
#include <QList>

class ConfigurationMerger {
	T_LOGGER_DECLARE(ConfigurationMerger);
public:
	ConfigurationMerger(QList<YamlParser*> baseFiles, QList<YamlParser*> overrideFiles);
	~ConfigurationMerger();
	void merge();
	std::pair<std::string, int> dump();

private:
	QList<YamlParser*> blobs;
	QList<xconfig::XConfigBucket*> bucketList;
	QList<const char*> stringPools;
	size_t firstOverride;
	std::vector<xconfig::XConfigBucket> destBuckets;
	std::vector<char> destStringPool;
	std::vector<char*> destKeys;
	std::vector<xconfig::XConfigBucket> dynamicBuckets;
	std::vector<char> dynamicStringPool;
	std::vector<char*> dynamicKeys;
	static const int kPerfectHashMaxTries = 5;

	void mergeNode(size_t blobId, size_t nodeId, size_t destBlobId, size_t parentInDestination);
	void replace(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId, size_t origBlob, size_t origNodeId);
	void insert(size_t parentBlob, size_t parentNodeId, size_t origBlob, size_t origNodeId);
	void erase(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId);
	xconfig::XConfigBucket* getBucket(size_t blobId, size_t nodeId);
	const char* getString(size_t blobId, size_t stringOffset);
	const char* getKey(size_t blobId, size_t nodeId);
	size_t findChild(size_t blobId, size_t parentNodeId, const std::string& name);
	int dumpNode(size_t nodeId, bool inMap);
	void expandRefs();
	void expandRef(size_t blobId, size_t nodeId);
	size_t insertDynamicString(const char* string);
	size_t insertDynamicBucket(const xconfig::XConfigBucket* bucket);
	size_t deepCopy(size_t nodeId, bool inMap, const std::string& keyPrefix);
};

#endif
