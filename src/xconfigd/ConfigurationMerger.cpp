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
#include <set>
#include <QString>

#include <boost/lexical_cast.hpp>

using std::string;
using boost::lexical_cast;
using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigHeader;
using xconfig::XConfigBucket;
using xconfig::XConfigValueType;

T_LOGGER_DEFINE(ConfigurationMerger, "ConfigurationMerger");

ConfigurationMerger::ConfigurationMerger(QList<YamlParser*> baseBlobs, QList<YamlParser*> overrideBlobs)
	: blobs(baseBlobs), firstOverride(baseBlobs.count())
{
	if (blobs.size() == 0) {
		TWARN("Number of base files is zero");
	}
	blobs << overrideBlobs;
	if (blobs.size() > 1) {
		for (auto it = blobs.begin(); it != blobs.end(); ++it) {
			TTRACE("ConfigurationMerger::ConfigurationMerger getHeader()");
			XConfigHeader header = (*it)->getHeader();
			XConfigBucket* buckets = new XConfigBucket[header.numBuckets];
			memcpy(buckets, (*it)->getBuckets(), sizeof(XConfigBucket) * header.numBuckets);
			bucketList << buckets;
			stringPools << (*it)->getStringPool();
			TTRACE("adding stringPool [%s]", &(*it)->getStringPool()[0]);
		}
	} else if (blobs.size() > 0) {
		auto it = blobs.begin();
		XConfigHeader header = (*it)->getHeader();
		XConfigBucket* buckets = const_cast<XConfigBucket*>((*it)->getBuckets());
		bucketList << buckets;
		stringPools << (*it)->getStringPool();
	} else {
		TWARN("Number of total files is zero");
		XConfigBucket* buckets = new XConfigBucket[1];
		buckets[0].type = xconfig::TYPE_MAP;
		buckets[0].parent = 0;
		buckets[0].name = 0;
		buckets[0].next = 0;
		buckets[0].value._vectorial.child = 0;
		buckets[0].value._vectorial.size = 0;
		bucketList << buckets;
		dynamicKeys.push_back(strdup(""));
	}
	// Additional entry on bucketList and stringPools for dynamic buckets generated from expandrefs
	bucketList << &dynamicBuckets[0];
	stringPools << &dynamicStringPool[0];
}

ConfigurationMerger::~ConfigurationMerger()
{
	// ignore last entry that points to dynamicBuckets
	bucketList.pop_back();
	if (blobs.size() != 1) {
		for (auto it = bucketList.begin(); it != bucketList.end(); ++it) {
			delete[](*it);
		}
	}
	for (auto key = dynamicKeys.begin(); key != dynamicKeys.end(); ++key) {
		free(*key); // allocated with strdup()
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

	if (int(blobId) > blobs.size()) {
		TTRACE("getKey(%ld,%ld) dynamicKeys.size()=%ld [%s]", blobId, nodeId, dynamicKeys.size(), dynamicKeys[nodeId - 1]);
	}

	return int(blobId) > blobs.size()
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
	TTRACE("findChild %ld %ld [%s]", parentBlobId, parentNodeId, name.c_str());
	// TODO use the hash function as optimization when the number of childs is huge
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);
	size_t n = 0;
	size_t childId = parentBucket->value._vectorial.child;
	size_t blobId = parentBlobId;
	for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
		canonicalIds(&blobId, &childId);
		XConfigBucket* childBucket;
		childBucket = getBucket(blobId, childId);
		TTRACE("child %ld [%s]", childId, getString(blobId, childBucket->name));
		if (name == getString(blobId, childBucket->name)) {
			canonicalIds(&blobId, &childId);
			return childId;
		}
		childId = childBucket->next;
	}
	return 0;
}

void ConfigurationMerger::replace(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId, size_t origBlob, size_t origNodeId) {
	TTRACE("replace parent=[%s] dest=[%s] orig=[%s]", getKey(parentBlobId, parentNodeId), getKey(destBlobId, destNodeId), getKey(origBlob, origNodeId));
	canonicalIds(&destBlobId, &destNodeId);
	XConfigBucket* origBucket = getBucket(origBlob, origNodeId);
	XConfigBucket* destBucket = getBucket(destBlobId, destNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);

	origBucket->next = destBucket->next ? composeNodeId(destBlobId, destBucket->next) : 0;

	size_t childId;
	canonicalIds(&destBlobId, &childId, destBlobId, parentBucket->value._vectorial.child);
	if (childId == destNodeId) {
		TTRACE("replace first of %d child", parentBucket->value._vectorial.size);
		parentBucket->value._vectorial.child = composeNodeId(origBlob, origNodeId);
	} else {
		size_t n = 0;
		for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
			XConfigBucket* childBucket = getBucket(destBlobId, childId);
			if (composeNodeId(destBlobId, childBucket->next) == destNodeId) {
				TTRACE("replace %ldth of %d child", n + 1, parentBucket->value._vectorial.size);
				childBucket->next = composeNodeId(origBlob, origNodeId);
				break;
			}
			childId = childBucket->next;
			canonicalIds(&destBlobId, &childId);
		}
		if (!childId || n >= parentBucket->value._vectorial.size) {
			TERROR("childs are corrupted for [%s]", getKey(parentBlobId, parentNodeId));
			// TODO throw
			throw std::exception();
		}
	}
}

void ConfigurationMerger::erase(size_t parentBlobId, size_t parentNodeId, size_t destBlobId, size_t destNodeId) {
	TTRACE("erase parent=[%s] dest=[%s]", getKey(parentBlobId, parentNodeId), getKey(destBlobId, destNodeId));
	canonicalIds(&destBlobId, &destNodeId);
	XConfigBucket* destBucket = getBucket(destBlobId, destNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentNodeId);

	size_t nextId = destBucket->next ? composeNodeId(destBlobId, destBucket->next) : 0;

	size_t childId;
	canonicalIds(&destBlobId, &childId, destBlobId, parentBucket->value._vectorial.child);
	if (childId == destNodeId) {
		TTRACE("replace first of %d child", parentBucket->value._vectorial.size);
		parentBucket->value._vectorial.child = nextId;
	} else {
		size_t n = 0;
		for (n = 0; n < parentBucket->value._vectorial.size && childId; n++) {
			XConfigBucket* childBucket = getBucket(destBlobId, childId);
			if (composeNodeId(destBlobId, childBucket->next) == destNodeId) {
				TTRACE("replace %ldth of %d child", n + 1, parentBucket->value._vectorial.size);
				childBucket->next = nextId;
				break;
			}
			childId = childBucket->next;
			canonicalIds(&destBlobId, &childId);
		}
		if (!childId || n >= parentBucket->value._vectorial.size) {
			TERROR("childs are corrupted for [%s]", getKey(parentBlobId, parentNodeId));
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
	TTRACE("insert parent=[%s] orig=[%s]", getKey(parentBlob, parentNodeId), getKey(origBlob, origNodeId));

	XConfigBucket* origBucket = getBucket(origBlob, origNodeId);
	XConfigBucket* parentBucket = getBucket(parentBlob, parentNodeId);

	assert(parentBucket->type == xconfig::TYPE_MAP);

	origBucket->parent = composeNodeId(parentBlob, parentNodeId);
	origBucket->next = parentBucket->value._vectorial.child ? composeNodeId(parentBlob, parentBucket->value._vectorial.child) : 0;
	parentBucket->value._vectorial.child = composeNodeId(origBlob, origNodeId);
	parentBucket->value._vectorial.size++;
	TTRACE("size=%d", parentBucket->value._vectorial.size);
}

void ConfigurationMerger::mergeNode(size_t blobId, size_t nodeId, size_t parentBlobId, size_t parentInDestination)
{
	canonicalIds(&blobId, &nodeId);
	canonicalIds(&parentBlobId, &parentInDestination);
	TTRACE("mergeNode %ld %ld %ld %ld [%s] [%s]", blobId, nodeId, parentBlobId, parentInDestination, getKey(blobId, nodeId), getKey(parentBlobId, parentInDestination));

	assert(nodeId != parentInDestination);

	XConfigBucket* currentBucket = getBucket(blobId, nodeId);
	XConfigBucket* parentBucket = getBucket(parentBlobId, parentInDestination);
	Q_UNUSED(parentBucket); // only used on TTRACE()

	TTRACE("current getString(%ld, %d)=[%s] [%s]", blobId, currentBucket->name, getString(blobId, currentBucket->name), getKey(blobId, nodeId));
	TTRACE("parent getString(%ld, %d)=[%s] [%s]", parentBlobId, parentBucket->name, getString(parentBlobId, parentBucket->name), getKey(parentBlobId,parentInDestination));

	size_t nodeInDestination;
	size_t destBlobId;
	if (decodeNodeId(nodeId) <= 1) {
		TTRACE("root node on merge");
		nodeInDestination = composeNodeId(1, 1);
		destBlobId = 1;
	} else {
		nodeInDestination = findChild(parentBlobId, parentInDestination, string(getString(blobId, currentBucket->name)));
		destBlobId = decodeBlobId(nodeInDestination);
	}
	TTRACE("parentInDestination %ld", parentInDestination);
	if (nodeInDestination) {
		if (currentBucket->type == xconfig::TYPE_MAP && getBucket(destBlobId, nodeInDestination)->type == xconfig::TYPE_MAP) {
			// iterate childen and call mergeNode on them with nodeInDestination as parentInDestination
			size_t i = 0;
			size_t childId = currentBucket->value._vectorial.child;
			TTRACE("iterate children %d", currentBucket->value._vectorial.size);
			for (i = 0; i < currentBucket->value._vectorial.size && childId; i++) {
				TTRACE("iterate # %ld", i);
				canonicalIds(&blobId, &childId);
				XConfigBucket* childBucket = getBucket(blobId, childId);
				auto nextChildId = childBucket->next;
				mergeNode(blobId, childId, destBlobId, nodeInDestination);
				// it's the same one again since we're inserting at front
				childId = nextChildId;
			}
			TTRACE("end iterate children %d", currentBucket->value._vectorial.size);
		} else {
			if (blobId > firstOverride) {
				TTRACE("replace %ld %ld %ld", nodeInDestination, blobId, nodeId);
				if (currentBucket->type == xconfig::TYPE_DELETE)
					erase(decodeBlobId(parentInDestination), parentInDestination, destBlobId, nodeInDestination);
				else
					replace(decodeBlobId(parentInDestination), parentInDestination, destBlobId, nodeInDestination, blobId, nodeId);
			} else {
				// warn: conflict found
				TWARN("conflict in base file %s:%s", blobs[blobId - 1]->getPath().c_str(), getKey(blobId, nodeId));
			}
		}
	} else {
		// add to destination as child of parentInDestination
		TTRACE("parentInDestination %ld", parentInDestination);
		TTRACE("insert %ld %ld %ld %ld", parentBlobId, parentInDestination, blobId, nodeId);
		if (currentBucket->type != xconfig::TYPE_DELETE)
			insert(parentBlobId, parentInDestination, blobId, nodeId);
	}
	TTRACE("mergeNode exit");
}

void ConfigurationMerger::merge()
{
	// First Blob is #1
	for (int blobId=2; blobId <= blobs.size(); blobId++) {
		TTRACE("merge blobId=%d", blobId);
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
		TTRACE("destKeys[%ld]=%s %p", i, destKeys[i], destKeys[i]);
	}
	cmph_io_adapter_t *source = cmph_io_vector_adapter(&destKeys[0], destKeys.size());
	TTRACE("source=%p", source);
	cmph_config_t *config = cmph_config_new(source);
	cmph_config_set_algo(config, CMPH_CHM);
	TTRACE("config=%p", config);
	cmph_t *hash_serialization = cmph_new(config);
	if (!hash_serialization) {
		TDEBUG("cannot generate hash");
		std::set<string> keysSet;
		// change duplicated keys by random keys to avoid duplication
		// insert random istring in a vector so they live until the
		// hash generation is done
		std::vector<string> randomKeys;
		for (size_t i=0; i<destKeys.size(); i++) {
			if (keysSet.count(destKeys[i])) {
				TWARN("duplicated key: %s", destKeys[i]);
				randomKeys.push_back(std::string("duplicate key:") + lexical_cast<string>(i));
				destKeys[i] = const_cast<char*>(randomKeys.back().c_str());
			}
			keysSet.insert(destKeys[i]);
		}
		// retry
		cmph_config_destroy(config);
		config = cmph_config_new(source);
		cmph_config_set_algo(config, CMPH_CHM);
		hash_serialization = cmph_new(config);
		if (!hash_serialization) {
			TERROR("cannot generate hash");
			abort();
		}
	}
	TTRACE("hash_serialization=%p", hash_serialization);
	cmph_config_destroy(config);

	XConfigHeader header;
	size_t hashSize = cmph_packed_size(hash_serialization);
	// align on 16 bytes boundary
	hashSize = ((hashSize - 1) / 16 + 1) * 16;
	size_t numBuckets = destBuckets.size();
	size_t stringPoolSize = destStringPool.size();
	size_t totalSize = sizeof(header) + hashSize + sizeof(XConfigBucket) * numBuckets + stringPoolSize;
	size_t offset = 0;

	TTRACE("hashSize=%ld numBuckets=%ld bucketSize=%ld stringPoolSize=%ld", hashSize, numBuckets, sizeof(XConfigBucket) * numBuckets, stringPoolSize);
	//TTRACE("destStringPool [");
	//for (size_t i = 0; i < stringPoolSize; i++) {
	//	TTRACE("%c", destStringPool[i] > 0 ? destStringPool[i] : '\n');
	//}
	//TTRACE("]\n");

	char xcFileName[] = "/tmp/xconfig-XXXXXX";
	int xcFd = mkstemp(xcFileName);
	if (xcFd < 0) {
		TERROR("mkstemp %s", strerror(errno));
		abort();
	}
	TTRACE("xcFd %d", xcFd);
	int truncResult = ftruncate(xcFd, totalSize);
	if (truncResult < 0) {
		TERROR("fruncate %s", strerror(errno));
		abort();
	}
	TTRACE("truncResult %d", truncResult);
	char* blob = (char*)mmap(0, totalSize, PROT_READ|PROT_WRITE, MAP_SHARED, xcFd, 0);
	TTRACE("blob %p", blob);

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
	TTRACE("dump destBuckets[%ld] [%s] inMap=%d", destBuckets.size(), key, inMap);

	destBuckets.push_back(*currentBucket);
	XConfigBucket* destBucket = &destBuckets.back();
	destKeys.push_back(const_cast<char*>(key));

	switch (currentBucket->type) {
		case xconfig::TYPE_MAP:
			isMap = true;
			// fall-through
		case xconfig::TYPE_SEQUENCE: {
			destBucket->value._vectorial.child = destBuckets.size() + 1;

			TTRACE(" vect size=%d, child=%d isMap=%d", destBucket->value._vectorial.size, destBucket->value._vectorial.child, isMap);

			size_t childId = currentBucket->value._vectorial.child;
			size_t parentDestId = nodeId ? destBucket->value._vectorial.child - 1 : 0;
			size_t n;
			for (n = 0; n < currentBucket->value._vectorial.size && childId; n++) {
				TTRACE("#%ld/%d ", n, currentBucket->value._vectorial.size);
				XConfigBucket* childBucket = getBucket(blobId, childId);
				int dumpedChildId = destBuckets.size();
				dumpNode(composeNodeId(blobId, childId), isMap);
				destBuckets[dumpedChildId].parent = parentDestId;
				TTRACE("parentDestId=%ld", parentDestId);
				childId = childBucket->next;
			}

			TTRACE("end vect %ld", nodeId ? parentDestId - 1 : 0);

			// recalculate afer insertions on the vector
			destBucket = &destBuckets[parentDestId ? parentDestId - 1 : 0];
			if (n != destBucket->value._vectorial.size) {
				TWARN("wrong size, fixing to %ld", n);
				destBucket->value._vectorial.size = n;
			}
			break;
		}
		case xconfig::TYPE_STRING: {
			char* name = const_cast<char*>(getString(blobId, destBucket->value._string));
			destBucket->value._string = destStringPool.size() + 1;
			destStringPool.insert(destStringPool.end(), name, name + strlen(name) + 1);
			TTRACE("scalar[%s]", name);
			break;
		}
		case xconfig::TYPE_EXPANDSTRING:
			// string is already in destStringPool
			TTRACE("expandstring[%s]", &destStringPool[destBucket->value._string - 1]);
			destBucket->type = xconfig::TYPE_STRING;
			break;
		case xconfig::TYPE_EXPANDREF: {
			// dump child
			TTRACE("expandref");
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
		TTRACE("name=[%s]", name);
	} else {
		TTRACE("name=%d", destBucket->name);
	}

	destBucket->next = currentBucket->next && currentBucket->parent ? destBuckets.size() + 1: 0;

	TTRACE("next=%d", destBucket->next);

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
		TTRACE("expanding expandref [%s]", getKey(blobId, nodeId));
		size_t refBlobId = 1;
		size_t refNodeId = 1;
		size_t childId = bucket->value._vectorial.child;
		size_t n;
		for (n = 0; n < bucket->value._vectorial.size && childId; n++) {
			TTRACE("#%ld/%d", n, bucket->value._vectorial.size);
			XConfigBucket* childBucket = getBucket(blobId, childId);
			assert(childBucket->type == xconfig::TYPE_STRING);
			refNodeId = findChild(refBlobId, refNodeId, getString(blobId, childBucket->value._string));
			canonicalIds(&refBlobId, &refNodeId);
			if (!refNodeId) {
				TTRACE("can't find ref");
				return;
			}
			childId = childBucket->next;
		}
		TTRACE("found");
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
		TTRACE("expanding expandstring [%s]", getKey(blobId, nodeId));
		XConfigBucket* stringBucket = getBucket(blobId, bucket->value._vectorial.child);
		assert(stringBucket->type == xconfig::TYPE_STRING);
		QString expanded = getString(blobId, stringBucket->value._string);
		size_t childId = stringBucket->next;
		for (size_t n=1; n < bucket->value._vectorial.size && childId; n++) {
			TTRACE(" getting ref #%ld", n);
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
					TTRACE("can't find ref");
					return;
				}
				child2ndLevelId = child2ndLevelBucket->next;
			}
			XConfigBucket* refBucket = getBucket(refBlobId, refNodeId);
			TTRACE("found ref #%ld [%s]", n, getString(refBlobId, refBucket->value._string));
			expanded = expanded.arg(getString(refBlobId, refBucket->value._string));

			childId = childBucket->next;
		}
		// string value is inserted into destStringPool
		bucket->value._string = destStringPool.size() + 1;
		auto value = expanded.toLocal8Bit();
		destStringPool.insert(destStringPool.end(), value.constData(), value.constData() + expanded.length() + 1);
	} else {
		TWARN("expanding unexpected type [%s]", getKey(blobId, nodeId));
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
	TTRACE("deepCopy(%ld,%d,%s) destBlobId=%ld destNodeId=%ld", nodeId, inMap, key.c_str(), destBlobId, destNodeId);
	if (inMap) {
		destBucket->name = insertDynamicString(getString(blobId, bucket->name));
	}
	dynamicKeys.push_back(strdup(key.c_str()));
	TTRACE("buckets.size()=%ld keys.size()=%ld [%s]", dynamicBuckets.size(), dynamicKeys.size(), key.c_str());
	switch (bucket->type) {
		case xconfig::TYPE_MAP:
			isMap = true;
			// fall-through
		case xconfig::TYPE_SEQUENCE: {
			// iterate
			TTRACE("vect size=%d, child=%d isMap=%d", destBucket->value._vectorial.size, destBucket->value._vectorial.child, isMap);

			size_t childId = bucket->value._vectorial.child;
			size_t firstChildId = 0;
			size_t prevChildId = 0;
			size_t n;
			for (n = 0; n < bucket->value._vectorial.size && childId; n++) {
				TTRACE("#%ld/%d", n, bucket->value._vectorial.size);

				canonicalIds(&blobId, &childId);
				XConfigBucket* childBucket = getBucket(blobId, childId);

				string childKey(key);
				if (!key.empty())
					childKey += isMap ? string("/") : string("#");
				childKey += isMap ? getString(blobId, childBucket->name) : lexical_cast<string>(childBucket->name);

				auto newChildId = deepCopy(composeNodeId(blobId, childId), isMap, childKey);
				if (prevChildId) {
					auto prevChildBlobId = decodeBlobId(prevChildId);
					TTRACE("prevChild(%ld)->next=%ld", prevChildId, newChildId);
					getBucket(prevChildBlobId, prevChildId)->next = newChildId;
				} else {
					firstChildId = newChildId;
				}
					
				prevChildId = newChildId;
				childId = childBucket->next;
			}

			TTRACE("end vect");

			// recalculate afer insertions on the vector
			TTRACE("recalculate destBucket %ld,%ld", destBlobId, destNodeId);
			auto destBucket = getBucket(destBlobId, destNodeId);
			if (n != destBucket->value._vectorial.size) {
				TWARN("wrong size, fixing to %ld", n);
				destBucket->value._vectorial.size = n;
			}
			destBucket->value._vectorial.child = firstChildId;
			break;
		}
		case xconfig::TYPE_STRING: {
			const char* value = getString(blobId, bucket->value._string);
			destBucket->value._string = insertDynamicString(value);
			TTRACE("deep copy string [%s] [%s]", value, getString(destBlobId, destBucket->value._string));
			break;
		}
		case xconfig::TYPE_DELETE:
			TWARN("Unexpected delete node");
			destBucket->type = xconfig::TYPE_NULL;
			break;
		case xconfig::TYPE_EXPANDREF:
		case xconfig::TYPE_EXPANDSTRING:
			TWARN("Multi level references are not allowed");
			destBucket->type = xconfig::TYPE_NULL;
			break;
		default:
			// non-string scalar. nothing further to be done
			break;
	}
	TTRACE("end deepCopy [%s] %ld", key.c_str(), dynamicKeys.size());
	// TODO insert key
	return destNodeId;
}
