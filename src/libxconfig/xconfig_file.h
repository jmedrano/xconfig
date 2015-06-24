#ifndef LIBXCONFIG_XCONFIG_FILE_H_
#define LIBXCONFIG_XCONFIG_FILE_H_

namespace xconfig {

enum XConfigValueType : uint32_t {
	TYPE_NULL,
	TYPE_BOOLEAN,
	TYPE_INTEGER,
	TYPE_FLOAT,
	TYPE_STRING,
	TYPE_MAP,
	TYPE_SEQUENCE,

	// virtual types
	// they can't be in a dumped file
	// they're temporary items only used within the daemon
	TYPE_EXPANDREF,
	TYPE_EXPANDSTRING,
	TYPE_DELETE,
	TYPE_MAP_OVERRIDED,
	TYPE_EXPANSION_IN_PROGRESS,
	TYPE_EXPANDREF_EXPANDED,
	TYPE_EXPANDSTRING_EXPANDED,
};


/**
 * Each bucket contains the type information (scalar types, sequences,
 * references, etc...).
 * Numeric scalar values are stored directly in the value union
 * structure in the bucket.
 *
 * Strings are stored elsewhere and the starting offset of the string
 * is referenced from the XConfigBuckets (both for .name and .value._string).
 *
 * In case of sequences (lists and maps) _vectorial.size is the length
 * of the sequence, and ._vectorial.child points to the first child.
 * Childs are chained together using the .next field.
 */
struct XConfigBucket {
	union {
		bool _boolean;
		int64_t _integer;
		uint64_t _string;
		double _float;
		struct {
			uint32_t size;
			uint32_t child;
		} _vectorial;
	} value;
	uint32_t name;
	XConfigValueType type;
	uint32_t parent;
	uint32_t next;
	uint32_t mtimeSecs;
	uint32_t mtimeNsecs;
};
#ifdef __GXX_EXPERIMENTAL_CXX0X__
static_assert(sizeof(XConfigBucket) == 32, "sizeof(XConfigBucket) != 32");
#endif

struct XConfigHeader {
	uint64_t hashSize;
	uint64_t numBuckets;
};
#ifdef __GXX_EXPERIMENTAL_CXX0X__
static_assert(sizeof(XConfigHeader) == 16, "sizeof(XConfigHeader) != 16");
#endif

inline bool isScalar(XConfigValueType type) {
	return type < TYPE_MAP;
}

} // namespace xconfig

#endif // LIBXCONFIG_XCONFIG_FILE_H_
