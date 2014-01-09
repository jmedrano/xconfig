#ifndef _HANDLE_H_INCLUDED_
#define _HANDLE_H_INCLUDED_

#include <string>
#include <xconfig/xconfig.h>
using std::string;
using xconfig::XConfig;
using xconfig::UnixConnectionPool;

jfieldID getHandleField(JNIEnv *env, jobject obj, string fieldName)
{
    jclass c = env->GetObjectClass(obj);
    // J is the type signature for long:
    return env->GetFieldID(c, fieldName.c_str(), "J");
}

template <typename T>
T *getHandle(JNIEnv *env, jobject obj, string fieldName)
{
    jlong handle = env->GetLongField(obj, getHandleField(env, obj, fieldName));
    return reinterpret_cast<T *>(handle);
}

template <typename T>
void setHandle(JNIEnv *env, jobject obj, T *t, string fieldName)
{
    jlong handle = reinterpret_cast<jlong>(t);
    env->SetLongField(obj, getHandleField(env, obj, fieldName), handle);
}

void saveXConfig(JNIEnv* environment, jobject object, XConfig* instance) {
  setHandle(environment, object, instance, "nativeHandle");
}

XConfig* getXConfig(JNIEnv* environment, jobject object) {
  return getHandle<XConfig>(environment, object, "nativeHandle");
}

#endif
