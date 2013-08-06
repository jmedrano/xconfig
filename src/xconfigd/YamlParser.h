#ifndef YAML_PARSER_H
#define YAML_PARSER_H

#include <xconfig.h>
#include <xconfig_file.h>
#include <yaml.h>
#include <time.h>

#include <string>
#include <vector>

class YamlParser {
public:
	YamlParser(std::string path);
	~YamlParser();
	void parse();
	size_t getTotalSize() const;
	const xconfig::XConfigHeader& getHeader() const;
	const xconfig::XConfigBucket* getBuckets() const;
	size_t getNumBuckets() const;
	const char* getStringPool() const;
	size_t getStringPoolSize() const;
	const std::vector<char*>& getKeys() const;

private:
	static const int RESERVE_BUCKETS = 1024;
	static const int MAX_BUCKETS = 65536;
	static const int RESERVE_STRINGPOOL = 16384;

	std::string path;
	std::vector<xconfig::XConfigBucket> buckets;
	std::vector<char> stringPool;
	std::vector<char*> keys;
	size_t bucketIdx;
	size_t stringOffset;
	yaml_parser_t* parser;
	xconfig::XConfigHeader header;
	size_t totalSize;
	struct timespec mtime;

	int yamlParseNode(const std::string& prefix, bool isDocumentRoot, bool isMapping, int maxItems = -1);
	xconfig::XConfigBucket* insertBucket(const std::string& key);

};

#endif
