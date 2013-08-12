#include "ConfigurationMerger.h"

#include <xconfig.h>
#include <xconfig_file.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <yaml.h>
#include <cmph.h>
#include <string>
#include <QString>

#include <boost/lexical_cast.hpp>

using std::string;
using boost::lexical_cast;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigHeader;
using xconfig::XConfigBucket;
using xconfig::XConfigValueType;

ConfigurationMerger::ConfigurationMerger(QList<YamlParser*> baseBlobs, QList<YamlParser*> overrideBlobs)
	: blobs(baseBlobs), firstOverride(baseBlobs.count())
{
	assert(blobs.size() > 0);
	blobs << overrideBlobs;
	if (blobs.size() > 1) {
		for (auto it = blobs.begin(); it != blobs.end(); ++it) {
printf("ConfigurationMerger::ConfigurationMerger getHeader()\n");
			XConfigHeader header = (*it)->getHeader();
			XConfigBucket* buckets = new XConfigBucket[header.numBuckets];
			memcpy(buckets, (*it)->getBuckets(), sizeof(XConfigBucket) * header.numBuckets);
			bucketList << buckets;
			stringPools << (*it)->getStringPool();
printf("adding stringPool [%s]\n", &(*it)->getStringPool()[0]);
		}
	} else {
		auto it = blobs.begin();
		XConfigHeader header = (*it)->getHeader();
		XConfigBucket* buckets = const_cast<XConfigBucket*>((*it)->getBuckets());
		bucketList << buckets;
		stringPools << (*it)->getStringPool();
	}
	// Additional entry on bucketList and stringPools for dynamic buckets generated from expandrefs
	bucketList << &dynamicBuckets[0];
	stringPools << &dynamicStringPool[0];
}

ConfigurationMerger::~ConfigurationMerger()
{
	bucketList.pop_back();
	if (blobs.size() > 1) {
		for (auto it = bucketList.begin(); it != bucketList.end(); ++it) {
			delete[](*it);
		}
	}
}

static inline size_t decodeNodeId(size_t composedId) {
	return composedId & 0xffff;
}

static inline size_t decodeBlobId(size_t composedId) {
	return composedId >> 16;
}

static inline size_t composeNodeId(size_t blobId, size_t nodeId) {
	size_t encodedBlobId = decodeBlobId(nodeId);
	if (encodedBlobId)
		blobId = encodedBlobId;
	return nodeId | (blobId << 16);
}

static inline void canonicalIds(size_t* canonicBlobId, size_t* canonicNodeId, size_t referenceBlobId, size_t encodedNodeId) {
	size_t decodedNodeId = decodeNodeId(encodedNodeId);
	size_t decodedBlobId = decodeBlobId(encodedNodeId);
	*canonicBlobId = decodedBlobId ? decodedBlobId : referenceBlobId;
	*canonicNodeId = composeNodeId(*canonicBlobId, decodedNodeId);
}

static inline void canonicalIds(size_t* blobId, size_t* nodeId) {
	canonicalIds(blobId, nodeId, *blobId, *nodeId);
}

const char* ConfigurationMerger::getKey(size_t blobId, size_t nodeId) {
	nodeId = decodeNodeId(nodeId);

	assert(nodeId > 0);
	assert(blobId > 0);

if (blobId > blobs.size()) {
	printf("getKey(%d,%d) dynamicKeys.size()=%d ", blobId, nodeId, dynamicKeys.size());
	fflush(stdout);
	printf("%p ", dynamicKeys[nodeId - 1]);
	fflush(stdout);
	printf("%s\n", dynamicKeys[nodeId - 1]);
	fflush(stdout);
}
	return blobId > blobs.size()
		? dynamicKeys[nodeId - 1]
		: blobs[blobId - 1]->getKeys()[nodeId - 1];
}

xconfig::XConfigBucket* ConfigurationMerger::getBucket(size_t blobId, size_t nodeId)
{
	canonicalIds(&blobId, &nodeId);
	nodeId = decodeNodeId(nodeId);

	assert(nodeId > 0);
	assert(blobId > 0);

	return &bucketList[blobId - 1][nodeId - 1];
}

const char* ConfigurationMerger::getString(size_t blobId, size_t stringOffset)
{
	if (stringOffset == 0)
		return "";

	return &stringPools[blobId - 1][stringOffset - 1];
}

size_t ConfigurationMerger::findChild(size_t parentBlobId, size_t parentNodeId, const std::string& name)
{
printf("findChild %ld %ld [%s]\n", parentBlobId, parentNodeId, name.c_str());
	// TODO use the hash function as optimization when the number of childs is huge
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);
	size_t n = 0;
	size_t childId = parentBucket->value._vectorial.child;
	size_t blobId = parentBlobId;
	for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
		canonicalIds(&blobId, &childId);
		XConfigBucket* childBucket;
		childBucket = getBucket(blobId, childId);
		printf(" child %ld [%s]\n", childId, getString(blobId, childBucket->name));
		if (name == getString(blobId, childBucket->name)) {
			canonicalIds(&blobId, &childId);
			return childId;
		}
		childId = childBucket->next;
	}
	return 0;
}

void ConfigurationMerger::replace(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId, size_t origBlob, size_t origNodeId) {
printf("replace parent=[%s] dest=[%s] orig=[%s]\n", getKey(parentBlobId, parentNodeId), getKey(destBlobId, destNodeId), getKey(origBlob, origNodeId));
	canonicalIds(&destBlobId, &destNodeId);
	XConfigBucket* origBucket = getBucket(origBlob, origNodeId);
	XConfigBucket* destBucket = getBucket(destBlobId, destNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);

	origBucket->next = destBucket->next;

	size_t childId;
	canonicalIds(&destBlobId, &childId, destBlobId, parentBucket->value._vectorial.child);
	if (childId == destNodeId) {
printf("replace first of %d child\n", parentBucket->value._vectorial.size);
		parentBucket->value._vectorial.child = composeNodeId(origBlob, origNodeId);
	} else {
		size_t n = 0;
		for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
			XConfigBucket* childBucket = getBucket(destBlobId, childId);
			if (composeNodeId(destBlobId, childBucket->next) == destNodeId) {
printf("replace %dth of %d child\n", n + 1, parentBucket->value._vectorial.size);
				childBucket->next = composeNodeId(origBlob, origNodeId);
				break;
			}
			childId = childBucket->next;
			canonicalIds(&destBlobId, &childId);
		}
		if (!childId || n >= parentBucket->value._vectorial.size) {
printf("WAT\n");
			// TODO throw
			throw std::exception();
		}
	}
}

void ConfigurationMerger::erase(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId) {
printf("erase parent=[%s] dest=[%s]\n", getKey(parentBlobId, parentNodeId), getKey(destBlobId, destNodeId));
	canonicalIds(&destBlobId, &destNodeId);
	XConfigBucket* destBucket = getBucket(destBlobId, destNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);

	size_t nextId = destBucket->next ? composeNodeId(destBlobId, destBucket->next) : 0;

	size_t childId;
	canonicalIds(&destBlobId, &childId, destBlobId, parentBucket->value._vectorial.child);
	if (childId == destNodeId) {
printf("replace first of %d child\n", parentBucket->value._vectorial.size);
		parentBucket->value._vectorial.child = nextId;
	} else {
		size_t n = 0;
		for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
			XConfigBucket* childBucket = getBucket(destBlobId, childId);
			if (composeNodeId(destBlobId, childBucket->next) == destNodeId) {
printf("replace %dth of %d child\n", n + 1, parentBucket->value._vectorial.size);
				childBucket->next = nextId;
				break;
			}
			childId = childBucket->next;
			canonicalIds(&destBlobId, &childId);
		}
		if (!childId || n >= parentBucket->value._vectorial.size) {
printf("WAT\n");
			// TODO throw
			throw std::exception();
		}
	}
	parentBucket->value._vectorial.size--;
	assert(parentBucket->value._vectorial.size);
}

void ConfigurationMerger::insert(size_t parentBlob, size_t parentNodeId, size_t origBlob, size_t origNodeId) {
	canonicalIds(&parentBlob, &parentNodeId);
	canonicalIds(&origBlob, &origNodeId);
printf("insert parent=[%s] orig=[%s] ", getKey(parentBlob, parentNodeId), getKey(origBlob, origNodeId));

	XConfigBucket* origBucket = getBucket(origBlob, origNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlob, parentNodeId);

	assert(parentBucket->type == xconfig::TYPE_MAP);

	origBucket->parent = composeNodeId(parentBlob, parentNodeId);
	origBucket->next = parentBucket->value._vectorial.child ? composeNodeId(parentBlob, parentBucket->value._vectorial.child) : 0;
	parentBucket->value._vectorial.child = composeNodeId(origBlob, origNodeId);
	parentBucket->value._vectorial.size++;
printf("size=%d\n", parentBucket->value._vectorial.size);
}

void ConfigurationMerger::mergeNode(size_t blobId, size_t nodeId, size_t parentBlobId, size_t parentInDestination)
{
	canonicalIds(&blobId, &nodeId);
	canonicalIds(&parentBlobId, &parentInDestination);
printf("mergeNode %ld %ld %ld %ld [%s] [%s]\n", blobId, nodeId, parentBlobId, parentInDestination, getKey(blobId, nodeId), getKey(parentBlobId, parentInDestination));

	assert(nodeId != parentInDestination);

	XConfigBucket* currentBucket = getBucket(blobId, nodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentInDestination);

if (nodeId)
printf(" current getString(%ld, %d)=[%s] [%s]\n", blobId, currentBucket->name, getString(blobId, currentBucket->name), getKey(blobId, nodeId));
if (parentInDestination)
printf(" parent getString(%ld, %d)=[%s] [%s]\n", parentBlobId, parentBucket->name, getString(parentBlobId, parentBucket->name), getKey(parentBlobId,parentInDestination));

	size_t nodeInDestination;
	size_t destBlobId;
	if (decodeNodeId(nodeId) <= 1) {
printf(" root node on merge ");
		nodeInDestination = composeNodeId(1, 1);
		destBlobId = 1;
	} else {
		nodeInDestination = findChild(parentBlobId, parentInDestination, string(getString(blobId, currentBucket->name)));
		destBlobId = decodeBlobId(nodeInDestination);
	}
printf(" parentInDestination %ld\n", parentInDestination);
	if (nodeInDestination) {
		if (currentBucket->type == xconfig::TYPE_MAP && getBucket(destBlobId, nodeInDestination)->type == xconfig::TYPE_MAP) {
			// iterate childen and call mergeNode on them with nodeInDestination as parentInDestination
			size_t i = 0;
			size_t childId = currentBucket->value._vectorial.child;
printf("iterate children %d\n", currentBucket->value._vectorial.size);
			for (i = 0; i < currentBucket->value._vectorial.size && childId; i++) {
printf("iterate # %d\n", i);
				canonicalIds(&blobId, &childId);
				XConfigBucket* childBucket = getBucket(blobId, childId);
				auto nextChildId = childBucket->next;
				mergeNode(blobId, childId, destBlobId, nodeInDestination);
				// it's the same one again since we're inserting at front
				childId = nextChildId;
			}
printf("end iterate children %d\n", currentBucket->value._vectorial.size);
		} else {
			if (blobId > firstOverride) {
printf("replace %ld %ld %ld\n", nodeInDestination, blobId, nodeId);
				if (currentBucket->type == xconfig::TYPE_DELETE)
					erase(decodeBlobId(parentInDestination), parentInDestination, destBlobId, nodeInDestination);
				else
					replace(decodeBlobId(parentInDestination), parentInDestination, destBlobId, nodeInDestination, blobId, nodeId);
			} else {
				// warn: conflict found
				printf("conflict in base file for [%s]\n", getKey(blobId, nodeId));
			}
		}
	} else {
		// add to destination as child of parentInDestination
printf(" parentInDestination %ld\n", parentInDestination);
printf("insert %ld %ld %ld %ld\n", parentBlobId, parentInDestination, blobId, nodeId);
		if (currentBucket->type != xconfig::TYPE_DELETE)
			insert(parentBlobId, parentInDestination, blobId, nodeId);
	}
printf("mergeNode exit\n");
}

void ConfigurationMerger::merge()
{
	// First Blob is #1
	for (int blobId=2; blobId <= blobs.size(); blobId++) {
		printf("merge blobId=%d\n", blobId);
		mergeNode(blobId, 1, 1, 1);
	}

}

std::pair<string, int> ConfigurationMerger::dump()
{
	destBuckets.reserve(16384);
	destStringPool.reserve(65536);
	destStringPool.push_back(0);
	dynamicStringPool.push_back(0);

	expandRefs();

	// First Blob is #1
	dumpNode(composeNodeId(1, 1), true);

	for (size_t i=0; i<destKeys.size(); i++) {
		printf("destKeys[%d]=%s %p\n", i, destKeys[i], destKeys[i]);
	}
	cmph_io_adapter_t *source = cmph_io_vector_adapter(&destKeys[0], destKeys.size());
printf("source=%p\n", source);
	cmph_config_t *config = cmph_config_new(source);
	cmph_config_set_algo(config, CMPH_CHM);
printf("config=%p\n", config);
	cmph_t *hash_serialization = cmph_new(config);
	if (!hash_serialization) {
		printf("cannot generate hash\n");
		abort();
	}
printf("hash_serialization=%p\n", hash_serialization);
	cmph_config_destroy(config);

	XConfigHeader header;
	size_t hashSize = cmph_packed_size(hash_serialization);
	size_t numBuckets = destBuckets.size();
	size_t stringPoolSize = destStringPool.size();
	size_t totalSize = sizeof(header) + hashSize + sizeof(XConfigBucket) * numBuckets + stringPoolSize;
	size_t offset = 0;

printf("hashSize=%d numBuckets=%d bucketSize=%d stringPoolSize=%d\n", hashSize, numBuckets, sizeof(XConfigBucket) * numBuckets, stringPoolSize);
printf("destStringPool [");
for (size_t i = 0; i < stringPoolSize; i++) {
	printf("%c", destStringPool[i] > 0 ? destStringPool[i] : '\n');
}
printf("]\n");

	char xcFileName[] = "/tmp/xconfig-XXXXXX";
	int xcFd = mkstemp(xcFileName);
	if (xcFd < 0) {
		perror("mkstemp");
		abort();
	}
	printf("xcFd %d\n", xcFd);
	int truncResult = ftruncate(xcFd, totalSize);
	if (truncResult < 0) {
		perror("fruncate");
		abort();
	}
	printf("truncResult %d\n", truncResult);
	char* blob = (char*)mmap(0, totalSize, PROT_READ|PROT_WRITE, MAP_SHARED, xcFd, 0);
	printf("blob %p\n", blob);

	header.hashSize = hashSize;
	header.numBuckets = numBuckets;
	memcpy(blob, &header, sizeof(header));
	offset += sizeof(header);
	assert(offset < totalSize);

	cmph_pack(hash_serialization, blob + offset);
	cmph_destroy(hash_serialization);
	cmph_io_vector_adapter_destroy(source);
	offset += hashSize;
	assert(offset < totalSize);

	memcpy(blob + offset, &destBuckets[0], sizeof(XConfigBucket) * numBuckets);
	offset += sizeof(XConfigBucket) * numBuckets;
	assert(offset < totalSize);

	memcpy(blob + offset, &destStringPool[0], destStringPool.size());
	assert(offset + stringPoolSize == totalSize);
	assert(destKeys.size() == destBuckets.size());

	return std::pair<string, int>(xcFileName, xcFd);
}

int ConfigurationMerger::dumpNode(size_t nodeId, bool inMap)
{
	size_t blobId = decodeBlobId(nodeId);
	XConfigBucket* currentBucket = getBucket(blobId, nodeId);
	const char *key = getKey(blobId, decodeNodeId(nodeId));
	bool isMap = false;
	printf("dump destBuckets[%d] [%s] inMap=%d", destBuckets.size(), key, inMap);

	destBuckets.push_back(*currentBucket);
	XConfigBucket* destBucket = &destBuckets.back();
	destKeys.push_back(const_cast<char*>(key));

	switch (currentBucket->type) {
		case xconfig::TYPE_MAP:
			isMap = true;
			// fall-through
		case xconfig::TYPE_SEQUENCE: {
			destBucket->value._vectorial.child = destBuckets.size() + 1;

			printf(" vect size=%d, child=%d isMap=%d\n", destBucket->value._vectorial.size, destBucket->value._vectorial.child, isMap);

			size_t childId = currentBucket->value._vectorial.child;
			size_t parentDestId = nodeId ? destBucket->value._vectorial.child - 1 : 0;
			size_t n;
			for (n = 0; n < currentBucket->value._vectorial.size && childId; n++) {
				printf("#%d/%d ", n, currentBucket->value._vectorial.size);
				XConfigBucket* childBucket = getBucket(blobId, childId);
				int dumpedChildId = destBuckets.size();
				dumpNode(composeNodeId(blobId, childId), isMap);
				destBuckets[dumpedChildId].parent = parentDestId;
printf("parentDestId=%d\n", parentDestId);
				childId = childBucket->next;
			}

			printf("end vect %d ", nodeId ? parentDestId - 1 : 0);

			// recalculate afer insertions on the vector
			destBucket = &destBuckets[parentDestId ? parentDestId - 1 : 0];
			if (n != destBucket->value._vectorial.size) {
				printf("wrong size, fixing to %d\n", n);
				destBucket->value._vectorial.size = n;
			}
			break;
		}
		case xconfig::TYPE_STRING: {
			char* name = const_cast<char*>(getString(blobId, destBucket->value._string));
			destBucket->value._string = destStringPool.size() + 1;
			destStringPool.insert(destStringPool.end(), name, name + strlen(name) + 1);
			printf(" scalar[%s] ", name);
			break;
		}
		case xconfig::TYPE_EXPANDSTRING:
			// string is already in destStringPool
			printf(" expandstring[%s] ", &destStringPool[destBucket->value._string - 1]);
			destBucket->type = xconfig::TYPE_STRING;
			break;
		case xconfig::TYPE_EXPANDREF: {
			// dump child
			printf("\n expandref \n");
			destBuckets.pop_back();
			destKeys.pop_back();
			return dumpNode(currentBucket->value._vectorial.child, inMap);
			break;
		}
		default:
			break;
	}

	if (inMap && nodeId && destBucket->name) {
		char* name = const_cast<char*>(getString(blobId, destBucket->name));
		destBucket->name = destStringPool.size() + 1;
		destStringPool.insert(destStringPool.end(), name, name + strlen(name) + 1);
		printf(" name=[%s] ", name);
	} else {
		printf(" name=%d ", destBucket->name);
	}

	destBucket->next = currentBucket->next && currentBucket->parent ? destBuckets.size() + 1: 0;

	printf(" next=%d\n", destBucket->next);

	return 0;
}

void ConfigurationMerger::expandRefs()
{
	int blobId = 1;
	for (auto blob = blobs.begin(); blob != blobs.end(); ++blob, ++blobId) {
		auto nodeIdsToBeExpanded = (*blob)->getNodeIdsToBeExpanded();
		for (auto nodeId = nodeIdsToBeExpanded.begin(); nodeId != nodeIdsToBeExpanded.end(); ++nodeId) {
			expandRef(blobId, *nodeId);
		}
	}
}

void ConfigurationMerger::expandRef(size_t blobId, size_t nodeId)
{
	canonicalIds(&blobId, &nodeId);
	XConfigBucket* bucket = getBucket(blobId, nodeId);
	if (bucket->type == xconfig::TYPE_EXPANDREF) {
		printf("expanding expandref [%s]\n", getKey(blobId, nodeId));
		size_t refBlobId = 1;
		size_t refNodeId = 1;
		size_t childId = bucket->value._vectorial.child;
		size_t n;
		for (n = 0; n < bucket->value._vectorial.size && childId; n++) {
			printf("#%d/%d ", n, bucket->value._vectorial.size);
			XConfigBucket* childBucket = getBucket(blobId, childId);
			assert(childBucket->type == xconfig::TYPE_STRING);
			refNodeId = findChild(refBlobId, refNodeId, getString(blobId, childBucket->value._string));
			canonicalIds(&refBlobId, &refNodeId);
			if (!refNodeId) {
				printf("can't find ref\n");
				return;
			}
			childId = childBucket->next;
		}
		printf("found\n");
		// TODO deep copy. keys need to be regenerated
		// we might need to use a new blobId to insert the copied buckets to
		if (true) {
			bucket->value._vectorial.child = deepCopy(refNodeId, /*inMap=*/true, getKey(blobId, nodeId));
			auto childBucket = getBucket(decodeBlobId(bucket->value._vectorial.child), bucket->value._vectorial.child);
			childBucket->name = insertDynamicString(getString(blobId, bucket->name));
		} else {
			bucket->type = xconfig::TYPE_STRING;
			bucket->value._string = 0;
		}
	} else if (bucket->type == xconfig::TYPE_EXPANDSTRING) {
		printf("expanding expandstring [%s]\n", getKey(blobId, nodeId));
		XConfigBucket* stringBucket = getBucket(blobId, bucket->value._vectorial.child);
		assert(stringBucket->type == xconfig::TYPE_STRING);
		QString expanded = getString(blobId, stringBucket->value._string);
		size_t childId = stringBucket->next;
		for (size_t n=1; n < bucket->value._vectorial.size && childId; n++) {
			printf(" getting ref #%d\n", n);
			// TODO get ref
			size_t refBlobId = 1;
			size_t refNodeId = 1;
			XConfigBucket* childBucket = getBucket(blobId, childId);
			assert(childBucket->type == xconfig::TYPE_SEQUENCE);
			size_t child2ndLevelId = childBucket->value._vectorial.child;
			for (size_t i = 0; i < childBucket->value._vectorial.size && child2ndLevelId; i++) {
				XConfigBucket* child2ndLevelBucket = getBucket(blobId, child2ndLevelId);
				assert(child2ndLevelBucket->type == xconfig::TYPE_STRING);
				refNodeId = findChild(refBlobId, refNodeId, getString(blobId, child2ndLevelBucket->value._string));
				canonicalIds(&refBlobId, &refNodeId);
				if (!refNodeId) {
					printf("can't find ref\n");
					return;
				}
				child2ndLevelId = child2ndLevelBucket->next;
			}
			XConfigBucket* refBucket = getBucket(refBlobId, refNodeId);
			printf("found ref #%d [%s]\n", n, getString(refBlobId, refBucket->value._string));
			expanded = expanded.arg(getString(refBlobId, refBucket->value._string));

			childId = childBucket->next;
		}
		// string value is inserted into destStringPool
		bucket->value._string = destStringPool.size() + 1;
		auto value = expanded.toLocal8Bit();
		destStringPool.insert(destStringPool.end(), value.constData(), value.constData() + expanded.length() + 1);
	} else {
		printf("expanding WAT [%s]\n", getKey(blobId, nodeId));
	}
}

size_t ConfigurationMerger::insertDynamicString(const char* string)
{
	auto stringOffset = dynamicStringPool.size();
	size_t len = strlen(string);
	dynamicStringPool.insert(dynamicStringPool.end(), string, string + len + 1);
	stringPools.back() = &dynamicStringPool[0];
	return stringOffset + 1;
}

size_t ConfigurationMerger::insertDynamicBucket(const XConfigBucket* bucket)
{
	dynamicBuckets.push_back(*bucket);
	bucketList.back() = &dynamicBuckets[0];
	return composeNodeId(bucketList.size(), dynamicBuckets.size());
}

size_t ConfigurationMerger::deepCopy(size_t nodeId, bool inMap, const string& key)
{
	auto blobId = decodeBlobId(nodeId);
	auto bucket = getBucket(blobId, nodeId);
	auto destNodeId = insertDynamicBucket(bucket);
	auto destBlobId = decodeBlobId(destNodeId);
	auto destBucket = getBucket(destBlobId, destNodeId);
	bool isMap = false;
printf("deepCopy(%d,%d,%s) destBlobId=%d destNodeId=%d\n", nodeId, inMap, key.c_str(), destBlobId, destNodeId);
	if (inMap) {
		destBucket->name = insertDynamicString(getString(blobId, bucket->name));
	}
	dynamicKeys.push_back(strdup(key.c_str()));
	printf("buckets.size()=%d keys.size()=%d [%s]\n", dynamicBuckets.size(), dynamicKeys.size(), key.c_str());
	switch (bucket->type) {
		case xconfig::TYPE_MAP:
			isMap = true;
			// fall-through
		case xconfig::TYPE_SEQUENCE: {
			// iterate
			printf(" vect size=%d, child=%d isMap=%d\n", destBucket->value._vectorial.size, destBucket->value._vectorial.child, isMap);

			size_t childId = bucket->value._vectorial.child;
			size_t firstChildId = 0;
			size_t prevChildId = 0;
			size_t n;
			for (n = 0; n < bucket->value._vectorial.size && childId; n++) {
				printf("#%d/%d ", n, bucket->value._vectorial.size);

				canonicalIds(&blobId, &childId);
				XConfigBucket* childBucket = getBucket(blobId, childId);

				string childKey(key);
				if (!key.empty())
					childKey += isMap ? string("/") : string("#");
				childKey += isMap ? getString(blobId, childBucket->name) : lexical_cast<string>(childBucket->name);
//printf(" childKey=[%s] ", childKey.c_str());

				auto newChildId = deepCopy(composeNodeId(blobId, childId), isMap, childKey);
				if (prevChildId) {
					auto prevChildBlobId = decodeBlobId(prevChildId);
					printf("\nprevChild(%d)->next=%d\n", prevChildId, newChildId);
					getBucket(prevChildBlobId, prevChildId)->next = newChildId;
				} else {
					firstChildId = newChildId;
				}
					
				prevChildId = newChildId;
				childId = childBucket->next;
			}

			printf("end vect\n");

			// recalculate afer insertions on the vector
			printf("recalculate destBucket %d,%d\n", destBlobId, destNodeId);
			auto destBucket = getBucket(destBlobId, destNodeId);
			if (n != destBucket->value._vectorial.size) {
				printf("wrong size, fixing to %d\n", n);
				destBucket->value._vectorial.size = n;
			}
			destBucket->value._vectorial.child = firstChildId;
			break;
		}
		case xconfig::TYPE_STRING: {
			const char* value = getString(blobId, bucket->value._string);
			//printf("deep copy string [%s]\n", value);
			destBucket->value._string = insertDynamicString(value);
			printf("deep copy string [%s] [%s]\n", value, getString(destBlobId, destBucket->value._string));
			break;
		}
		case xconfig::TYPE_DELETE:
			printf("Unexpected delete node\n");
			destBucket->type = xconfig::TYPE_NULL;
			break;
		case xconfig::TYPE_EXPANDREF:
		case xconfig::TYPE_EXPANDSTRING:
			printf("Multi level references are not allowed\n");
			destBucket->type = xconfig::TYPE_NULL;
			break;
		default:
			// non-string scalar. nothing further to be done
			break;
	}
printf("end deepCopy [%s] %d\n", key.c_str(), dynamicKeys.size());
	// TODO insert key
	return destNodeId;
}
