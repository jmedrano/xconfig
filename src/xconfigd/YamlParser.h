#ifndef YAML_PARSER_H
#define YAML_PARSER_H

#include <xconfig.h>
#include <xconfig_file.h>
#include <yaml.h>
#include <time.h>

#include <string>
#include <vector>
#include <set>
#include <exception>

#include "TLogger.h"

class YamlParser {
	T_LOGGER_DECLARE(YamlParser);
public:
	YamlParser(std::string path);
	~YamlParser();
	bool parse();
	size_t getTotalSize() const;
	const xconfig::XConfigHeader& getHeader() const;
	const xconfig::XConfigBucket* getBuckets() const;
	size_t getNumBuckets() const;
	const char* getStringPool() const;
	size_t getStringPoolSize() const;
	const std::vector<char*>& getKeys() const;
	const std::set<size_t>& getNodeIdsToBeExpanded() const;
	const std::string& getPath() const;

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
	int fd;
	FILE* file;
	yaml_parser_t* parser;
	xconfig::XConfigHeader header;
	size_t totalSize;
	struct timespec mtime;
	std::set<size_t> nodeIdsToBeExpanded;

	int yamlParseNode(const std::string& prefix, bool isDocumentRoot, bool isMapping, int maxItems = -1);
	xconfig::XConfigBucket* insertBucket(const std::string& key);
	void inferScalarType(xconfig::XConfigBucket* bucket, const char* value, const char* tag, yaml_scalar_style_t style);
	void parserError(const yaml_parser_t* parser);

};

class YamlNotFoundException: public std::exception {
};

class YamlSyntaxErrorException: public std::exception {
};

#endif
