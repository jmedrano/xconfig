#include <string>
#include <xconfig/xconfig.h>
#include <boost/algorithm/string.hpp>
#include "com_tuenti_xconfig_XConfigNative.h"
#include "handle.h"

using xconfig::XConfig;
using xconfig::XConfigNode;
using xconfig::XConfigNotFound;
using xconfig::XConfigNotConnected;
using xconfig::UnixConnectionPool;
using xconfig::UnixConnection;
using xconfig::XConfigValueType;
using std::string;
using std::vector;
using std::map;

static UnixConnectionPool* xconfig_pool;

/*
 * Library constructtor
 */
__attribute__((constructor))
static void init() {
  xconfig_pool = new UnixConnectionPool;
}

/**
 * Library destructor
 */
__attribute__((destructor))
static void fini() {
  delete xconfig_pool;
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
 * Generic method to create basic boxed type in java and overloaded without constructor params.
 */
template <typename T>
jobject createNewJobject(JNIEnv * environment, const string& javaClass, const string& constructorSignature, T value) {
  jclass cls;
  jmethodID methodId;
  cls = environment->FindClass(javaClass.c_str());
  if (cls != 0) {
    methodId = environment->GetMethodID(cls, "<init>", constructorSignature.c_str());
    if (methodId != 0) {
      if (constructorSignature == "()V") {
        return environment->NewObject(cls, methodId);
      } else {
        return environment->NewObject(cls, methodId, value);
      }
    }
  }
  return NULL;
}
jobject createNewJobjectWithEmptyConstructor(JNIEnv * environment, const string& javaClass) {
  jclass cls;
  jmethodID methodId;
  cls = environment->FindClass(javaClass.c_str());
  if (cls != 0) {
    methodId = environment->GetMethodID(cls, "<init>", "()V");
    if (methodId != 0) {
       return environment->NewObject(cls, methodId);
    }
  }
  return NULL;
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
  jclass cls;
  jmethodID methodId;
  jobject newObject;
  jstring stringValue;
  switch (type) {
    case XConfigValueType::TYPE_INTEGER:
      return createNewJobject(environment, "com/tuenti/xconfig/type/XConfigInteger", "(I)V", node.getInt());
    case XConfigValueType::TYPE_BOOLEAN:
      return createNewJobject(environment, "com/tuenti/xconfig/type/XConfigBoolean", "(Z)V", node.getBool());
    case XConfigValueType::TYPE_FLOAT:
      return createNewJobject(environment, "com/tuenti/xconfig/type/XConfigFloat", "(F)V", node.getFloat());
    case XConfigValueType::TYPE_STRING:
      stringValue = getJstringFromString(environment, node.getString());
      return createNewJobject(environment, "com/tuenti/xconfig/type/XConfigString", "(Ljava/lang/String;)V", stringValue);
    case XConfigValueType::TYPE_SEQUENCE:
      newObject = createNewJobjectWithEmptyConstructor(environment, "com/tuenti/xconfig/type/XConfigList");
      cls = environment->FindClass("com/tuenti/xconfig/type/XConfigList");
      if (newObject != NULL) {
        vector<XConfigNode> children = node.getChildren();
        for (vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it) {
          jobject newChild = getJobjectFromXConfigNode(environment, *it);
          methodId = environment->GetMethodID(cls, "add", "(Lcom/tuenti/xconfig/type/XConfigValue;)V");
          if (methodId != 0) {
            environment->CallVoidMethod(newObject, methodId, newChild);
          }
        }
      }
      return newObject;
    case XConfigValueType::TYPE_MAP:
      newObject = createNewJobjectWithEmptyConstructor(environment, "com/tuenti/xconfig/type/XConfigMap");
      cls = environment->FindClass("com/tuenti/xconfig/type/XConfigMap");
      if (newObject != NULL) {
        vector<XConfigNode> children = node.getChildren();
        for (vector<XConfigNode>::const_iterator it = children.begin(); it != children.end(); ++it) {
          jobject newChild = getJobjectFromXConfigNode(environment, *it);
          string name = it->getName();
          methodId = environment->GetMethodID(cls, "add", "(Ljava/lang/String;Lcom/tuenti/xconfig/type/XConfigValue;)V");
          if (methodId != 0) {
            environment->CallVoidMethod(newObject, methodId, getJstringFromString(environment, name), newChild);
          }
        }
      }
      return newObject;
    case XConfigValueType::TYPE_NULL:
      return createNewJobjectWithEmptyConstructor(environment, "com/tuenti/xconfig/type/XConfigNull");
    default:
      throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigUnsupportedNodeTypeException");
    }
    return NULL;
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
  } catch (XConfigNotConnected &e) {
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
  } catch (XConfigNotFound e) {
    throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigKeyNotFoundException", key);
    return NULL; // Return to get the Java exception processed
  } catch (XConfigNotConnected e) {
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
  } catch (XConfigNotFound e) {
    throwJavaException(environment, "com/tuenti/xconfig/exception/XConfigKeyNotFoundException", key);
    return 0; // Return to get the Java exception processed
  }
}
