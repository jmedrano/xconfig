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
}

ConfigurationMerger::~ConfigurationMerger()
{
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

	return blobs[blobId - 1]->getKeys()[nodeId - 1];
}

xconfig::XConfigBucket* ConfigurationMerger::getBucket(size_t blobId, size_t nodeId)
{
	size_t canonicNodeId;
	size_t canonicBlobId;
	canonicalIds(&blobId, &canonicNodeId, blobId, nodeId);
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
			canonicalIds(&destBlobId, &childId, destBlobId, parentBucket->value._vectorial.child);
		}
		if (!childId || n >= parentBucket->value._vectorial.size) {
printf("WAT\n");
			// TODO throw
			throw std::exception();
		}
	}
}

void ConfigurationMerger::erase(size_t destBlob, size_t destNodeId) {
	destNodeId = composeNodeId(destBlob, destNodeId);
	XConfigBucket* destBucket = getBucket(destBlob, destNodeId);
	size_t parentId = destBucket->parent;
	XConfigBucket* parentBucket = getBucket(destBlob, parentId);

	size_t childId = composeNodeId(destBlob, parentBucket->value._vectorial.child);
	if (childId == destNodeId) {
printf("erase first child\n");
		parentBucket->value._vectorial.child = destBucket->next;
	} else {
		size_t n = 0;
		for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
			XConfigBucket* childBucket = getBucket(destBlob, childId);
			if (composeNodeId(destBlob, childBucket->next) == destNodeId) {
printf("erase %dth child\n", n + 1);
				childBucket->next = destBucket->next;
				break;
			}
			childId = childBucket->next;
			if (decodeBlobId(childId)) {
				destBlob = decodeBlobId(childId);
			}
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
	origBucket->next = composeNodeId(parentBlob, parentBucket->value._vectorial.child);
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
		insert(parentBlobId, parentInDestination, blobId, nodeId);
	}
printf("mergeNode exit\n");
}

void ConfigurationMerger::merge()
{
	// First Blob is #1
	for (int blobId=2; blobId <= bucketList.size(); blobId++) {
		printf("merge blobId=%d\n", blobId);
		mergeNode(blobId, 1, 1, 1);
	}
}

std::pair<string, int> ConfigurationMerger::dump()
{
	destBuckets.reserve(16384);
	destStringPool.reserve(65536);
	destStringPool.push_back(0);

	// First Blob is #1
	dumpNode(composeNodeId(1, 1), true);

	for (int i=0; i<destKeys.size(); i++) {
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
for (int i = 0; i < stringPoolSize; i++) {
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
	XConfigBucket* destBucket = destBucket = &destBuckets.back();
	destKeys.push_back(const_cast<char*>(key));

	switch (currentBucket->type) {
		case xconfig::TYPE_MAP:
			isMap = true;
			// fall-through
		case xconfig::TYPE_SEQUENCE: {
			//destBucket->value._vectorial.child = destBuckets.size();
			destBucket->value._vectorial.child = destBuckets.size() + 1;

			printf(" vect size=%d, child=%d isMap=%d\n", destBucket->value._vectorial.size, destBucket->value._vectorial.child, isMap);

			size_t childId = currentBucket->value._vectorial.child;
			size_t parentDestId = nodeId ? destBucket->value._vectorial.child - 1 : 0;
			int n;
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
