#include <string>
#include <xconfig/xconfig.h>
#include <boost/algorithm/string.hpp>
#include "com_tuenti_xconfig_XConfigNative.h"
#include "cache_holders.h"
#include <limits>

using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigNotFound;
using xconfig::XConfigNotConnected;
using xconfig::UnixConnectionPool;
using xconfig::UnixConnection;
using xconfig::XConfigValueType;
using std::string;
using std::vector;

static UnixConnectionPool* xconfig_pool;
// Class cache
// com/tuenti/xconfig/type/XConfigNull
static XConfigClassHolder* xconfig_null_holder;
// com/tuenti/xconfig/type/XConfigBoolean
static XConfigClassHolder* xconfig_boolean_holder;
// com/tuenti/xconfig/type/XConfigInteger
static XConfigClassHolder* xconfig_integer_holder;
// com/tuenti/xconfig/type/XConfigLong
static XConfigClassHolder* xconfig_long_holder;
// com/tuenti/xconfig/type/XConfigFloat
static XConfigClassHolder* xconfig_float_holder;
// com/tuenti/xconfig/type/XConfigString
static XConfigClassHolder* xconfig_string_holder;
// com/tuenti/xconfig/type/XConfigList
static XConfigClassHolder* xconfig_list_holder;
// com/tuenti/xconfig/type/XConfigMap
static XConfigClassHolder* xconfig_map_holder;
// java/util/ArrayList
static CollectionHolder* arraylist_holder;
// java/util/HashMap
static CollectionHolder* hashmap_holder;
// native XConfig instance handle holder
static FieldHolder* handle_holder;

#define JNI_VERSION JNI_VERSION_1_8

/**
 * Called when the library is first loaded by a classloader
 */
jint JNI_OnLoad(JavaVM* vm, void* reserved) {
    xconfig_pool = new UnixConnectionPool;

    JNIEnv* env;
    if (vm->GetEnv(reinterpret_cast<void**>(&env), JNI_VERSION) != JNI_OK) {
        return JNI_ERR;
    }

    PRELOAD_CLASS(xconfig_null_holder, env, "com/tuenti/xconfig/type/XConfigNull", "()V");
    PRELOAD_CLASS(xconfig_boolean_holder, env, "com/tuenti/xconfig/type/XConfigBoolean", "(Z)V");
    PRELOAD_CLASS(xconfig_integer_holder, env, "com/tuenti/xconfig/type/XConfigInteger", "(I)V");
    PRELOAD_CLASS(xconfig_long_holder, env, "com/tuenti/xconfig/type/XConfigLong", "(J)V");
    PRELOAD_CLASS(xconfig_float_holder, env, "com/tuenti/xconfig/type/XConfigFloat", "(F)V");
    PRELOAD_CLASS(xconfig_string_holder, env, "com/tuenti/xconfig/type/XConfigString", "(Ljava/lang/String;)V");

    PRELOAD_COLLECTION(arraylist_holder, env, "java/util/ArrayList", "(I)V", "add", "(Ljava/lang/Object;)Z");
    PRELOAD_CLASS_FACTORY(xconfig_list_holder, env, "com/tuenti/xconfig/type/XConfigList", "wrapping", "java/util/List");
    PRELOAD_COLLECTION(hashmap_holder, env, "java/util/HashMap", "(I)V", "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
    PRELOAD_CLASS_FACTORY(xconfig_map_holder, env, "com/tuenti/xconfig/type/XConfigMap", "wrapping", "java/util/Map");

    PRELOAD_FIELD(handle_holder, env, "com/tuenti/xconfig/XConfigNative", "nativeHandle", "J");

    return JNI_VERSION;
}

/**
 * Called when all classes linked to this library are GCed
 */
void JNI_OnUnload(JavaVM *vm, void *reserved) {
    delete xconfig_pool;

    JNIEnv* env;
    vm->GetEnv(reinterpret_cast<void**>(&env), JNI_VERSION);

    FREE_HOLDER(xconfig_null_holder, env);
    FREE_HOLDER(xconfig_boolean_holder, env);
    FREE_HOLDER(xconfig_integer_holder, env);
    FREE_HOLDER(xconfig_long_holder, env);
    FREE_HOLDER(xconfig_float_holder, env);
    FREE_HOLDER(xconfig_string_holder, env);

    FREE_HOLDER(arraylist_holder, env);
    FREE_HOLDER(xconfig_list_holder, env);
    FREE_HOLDER(hashmap_holder, env);
    FREE_HOLDER(xconfig_map_holder, env);

    FREE_HOLDER(handle_holder, env);
}

/**
 * String/Jstring manipulation methods
 */
string getStringFromJstring(JNIEnv * environment, jstring originalString) {
  const char* charString = environment->GetStringUTFChars(originalString, NULL);
  string newString = string(charString);
  environment->ReleaseStringUTFChars(originalString, charString);
  return newString;
}

jstring getJstringFromString(JNIEnv* environment, string originalString) {
  jstring newString;
  newString = environment->NewStringUTF(originalString.c_str());
  return newString;
}

vector<string> getXConfigVectorFromString(const string& str) {
  vector<string> result;
  result = boost::split(result, str, boost::is_any_of("/"));
  return result;
}

/**
 * Auxiliar method to throw exceptions in java side given the exception class path.
 */
void throwJavaException(JNIEnv *environment, const string& exceptionPath, const string& message) {
  jclass cls = environment->FindClass(exceptionPath.c_str());
  environment->ThrowNew(cls, message.c_str());
  environment->DeleteLocalRef(cls);
}
void throwJavaException(JNIEnv *environment, const string& exceptionPath) {
  jclass cls = environment->FindClass(exceptionPath.c_str());
  environment->ThrowNew(cls, NULL);
  environment->DeleteLocalRef(cls);
}

/**
 * Auxiliar method to create Jobject depending on XConfigNode's type.
 */
jobject getJobjectFromXConfigNode(JNIEnv * environment, XConfigNode node) {
  XConfigValueType type = node.getType();
  switch (type) {
    case XConfigValueType::TYPE_INTEGER:
      {
        int64_t intValue = node.getInt();
        if (intValue < std::numeric_limits<int>::min() || intValue > std::numeric_limits<int>::max()) {
          return NEW_INSTANCE(xconfig_long_holder, environment, intValue);
        } else {
          return NEW_INSTANCE(xconfig_integer_holder, environment, intValue);
        }
      }
    case XConfigValueType::TYPE_BOOLEAN:
      return NEW_INSTANCE(xconfig_boolean_holder, environment, node.getBool());
    case XConfigValueType::TYPE_FLOAT:
      return NEW_INSTANCE(xconfig_float_holder, environment, node.getFloat());
    case XConfigValueType::TYPE_STRING:
      {
        jstring stringValue = getJstringFromString(environment, node.getString());
        return NEW_INSTANCE(xconfig_string_holder, environment, stringValue);
      }
    case XConfigValueType::TYPE_SEQUENCE:
      {
        vector<XConfigNode> children = node.getChildren();
        jobject collection = NEW_INSTANCE(arraylist_holder, environment, children.size());
        if (collection != NULL) {
          for (vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it) {
            jobject element = getJobjectFromXConfigNode(environment, *it);
            if (element == NULL) {
                return NULL;
            }
            LIST_ADD(arraylist_holder, environment, collection, element);
            environment->DeleteLocalRef(element);
          }
          return BUILD_INSTANCE(xconfig_list_holder, environment, collection);
        }
        return NULL;
      }
    case XConfigValueType::TYPE_MAP:
      {
        vector<XConfigNode> children = node.getChildren();
        jobject collection = NEW_INSTANCE(hashmap_holder, environment, children.size());
        if (collection != NULL) {
          for (vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it) {
            jstring key = getJstringFromString(environment, it->getName());
            jobject element = getJobjectFromXConfigNode(environment, *it);
            if (element == NULL) {
                return NULL;
            }
            MAP_PUT(hashmap_holder, environment, collection, key, element);
            environment->DeleteLocalRef(key);
            environment->DeleteLocalRef(element);
          }
          return BUILD_INSTANCE(xconfig_map_holder, environment, collection);
        }
        return NULL;
      }
    case XConfigValueType::TYPE_NULL:
      return NEW_INSTANCE(xconfig_null_holder, environment);
    default:
      throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigUnsupportedNodeTypeException");
    }
    return NULL;
}

static inline XConfig* getXConfig(JNIEnv* env, jobject object) {
    jlong raw_handle = GET_LONG_FIELD(handle_holder, env, object);
    return reinterpret_cast<XConfig*>(raw_handle);
}

static inline void saveXConfig(JNIEnv* env, jobject object, XConfig* instance) {
    jlong raw_handle = reinterpret_cast<jlong>(instance);
    SET_LONG_FIELD(handle_holder, env, object, raw_handle);
}

/**
 * Native glue methods implementation.
 */
JNIEXPORT void JNICALL Java_com_tuenti_xconfig_XConfigNative_init
(JNIEnv * environment, jobject object, jstring socket, jstring path, jboolean autoReload) {
  string pathStr = getStringFromJstring(environment, path);
  string socketStr = getStringFromJstring(environment, socket);
  try {
    XConfig *instance = new XConfig(xconfig_pool->getConnection(pathStr, socketStr), (bool) autoReload);
    saveXConfig(environment, object, instance);
  } catch (const XConfigNotConnected &e) {
      throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigConnectionFailedException");
  }
}

JNIEXPORT void JNICALL Java_com_tuenti_xconfig_XConfigNative_closeConnection
(JNIEnv * environment, jobject object) {
  XConfig *instance = getXConfig(environment, object);
  instance->close();
}

JNIEXPORT void JNICALL Java_com_tuenti_xconfig_XConfigNative_free
(JNIEnv * environment, jobject object) {
  XConfig *instance = getXConfig(environment, object);
  delete instance;
}


JNIEXPORT jboolean JNICALL Java_com_tuenti_xconfig_XConfigNative_reload
(JNIEnv * environment, jobject object) {
  XConfig *instance = getXConfig(environment, object);
  return (jboolean) instance->reload();
}

JNIEXPORT jobject JNICALL Java_com_tuenti_xconfig_XConfigNative_getValue
(JNIEnv * environment, jobject object, jstring inputKey) {
  XConfig *instance = getXConfig(environment, object);
  string key = getStringFromJstring(environment, inputKey);
  vector<string> keyVector = getXConfigVectorFromString(key);
  try {
    XConfigNode node = instance->getNode(keyVector);
    // Generate object depending on value's type
    return getJobjectFromXConfigNode(environment, node);
  } catch (const XConfigNotFound &e) {
    throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigKeyNotFoundException", key);
    return NULL; // Return to get the Java exception processed
  } catch (const XConfigNotConnected &e) {
    throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigNotConnectedException");
    return NULL; // Return to get the Java exception processed
  }
}

JNIEXPORT jlong JNICALL Java_com_tuenti_xconfig_XConfigNative_getLastModificationTime
(JNIEnv * environment, jobject object, jstring inputKey) {
  XConfig *instance = getXConfig(environment, object);
  string key = getStringFromJstring(environment, inputKey);
  vector<string> keyVector = getXConfigVectorFromString(key);
  try {
    struct timespec tm = instance->getMtime(keyVector);
    return tm.tv_sec * 1000LL + tm.tv_nsec / 1000000;
  } catch (const XConfigNotFound &e) {
    throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigKeyNotFoundException", key);
    return 0; // Return to get the Java exception processed
  }
}
