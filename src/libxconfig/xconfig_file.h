#ifndef _XCONFIG_FILE_H_
#define _XCONFIG_FILE_H_

enum XConfigValueType {
	XConfigTypeMap,
	XConfigTypeSequence,
	XConfigTypeBoolean,
	XConfigTypeInteger,
	XConfigTypeFloat,
	XConfigTypeString,
};

struct XConfigBucket {
	uint32_t name;
	enum XConfigValueType type;
	union {
		bool _boolean;
		uint64_t _integer;
		uint64_t _string;
		double _float;
		struct {
			uint32_t size;
			uint32_t child;
		} _vectorial;
	} value;
	uint32_t parent;
	uint32_t next;
	uint32_t mtime_secs;
	uint32_t mtime_nsecs;
};
#ifdef __GXX_EXPERIMENTAL_CXX0X__
static_assert(sizeof(XConfigBucket) == 32, "sizeof(XConfigBucket) != 32");
#endif

struct XConfigHeader {
	uint32_t hash_size;
	uint32_t num_buckets;
};
#ifdef __GXX_EXPERIMENTAL_CXX0X__
static_assert(sizeof(XConfigHeader) == 8, "sizeof(XConfigHeader) != 8");
#endif

#endif // _XCONFIG_FILE_H_
