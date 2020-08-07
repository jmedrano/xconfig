#ifndef _CACHE_HOLDERS_H_INCLUDED_
#define _CACHE_HOLDERS_H_INCLUDED_

#include <cstdarg>

#define PRELOAD_CLASS(holder, env, name, signature)      \
(holder) = loadXConfigClass((env), (name), (signature)); \
if ((holder) == NULL) {                                  \
    return JNI_ERR;                                      \
}

#define PRELOAD_COLLECTION(holder, env, name, constructor_signature, add_method, add_signature)         \
(holder) = loadCollectionHolder((env), (name), (constructor_signature), (add_method), (add_signature)); \
if ((holder) == NULL) {                                                                                 \
    return JNI_ERR;                                                                                     \
}

#define PRELOAD_CLASS_FACTORY(holder, env, name, method, source)               \
(holder) = loadXConfigClassByStaticFactory((env), (name), (method), (source)); \
if ((holder) == NULL) {                                                        \
    return JNI_ERR;                                                            \
}

#define PRELOAD_FIELD(holder, env, owner, name, type)       \
(holder) = loadFieldHolder((env), (owner), (name), (type)); \
if ((holder) == NULL) {                                     \
    return JNI_ERR;                                         \
}

#define FREE_HOLDER(holder, env)                \
(env)->DeleteGlobalRef((holder)->class_holder); \
delete (holder);

#define NEW_INSTANCE(holder, env, ...) \
(env)->NewObject((holder)->class_holder, (holder)->constructor_id, ##__VA_ARGS__)

#define BUILD_INSTANCE(holder, env, ...) \
safe_CallStaticObjectMethod((env), (holder)->class_holder, (holder)->constructor_id, ##__VA_ARGS__)

#define LIST_ADD(holder, env, list, element)                                     \
jboolean result = (env)->CallBooleanMethod((list), (holder)->add_id, (element)); \
if (result == JNI_FALSE || (env)->ExceptionCheck() == JNI_TRUE) {                \
    return NULL;                                                                 \
}

#define MAP_PUT(holder, env, map, key, value)                                    \
jobject prev = (env)->CallObjectMethod((map), (holder)->add_id, (key), (value)); \
if (prev != NULL) {                                                              \
    (env)->DeleteLocalRef(prev);                                                 \
}                                                                                \
if ((env)->ExceptionCheck() == JNI_TRUE) {                                       \
    return NULL;                                                                 \
}

#define GET_LONG_FIELD(holder, env, object) \
(env)->GetLongField((object), (holder)->field_id);

#define SET_LONG_FIELD(holder, env, object, value) \
(env)->SetLongField((object), (holder)->field_id, value);

struct XConfigClassHolder {
    jclass class_holder;
    jmethodID constructor_id;
};

struct CollectionHolder {
    jclass class_holder;
    jmethodID constructor_id;
    jmethodID add_id;
};

struct FieldHolder {
    jclass class_holder;
    jfieldID field_id;
};

static XConfigClassHolder* loadXConfigClass(JNIEnv* env, const char* name, const char* signature) {
    jclass local_class = env->FindClass(name);
    if (local_class == NULL) {
        return NULL;
    }
    jmethodID constructor_id = env->GetMethodID(local_class, "<init>", signature);
    if (constructor_id == NULL) {
        return NULL;
    }
    jclass global_class = reinterpret_cast<jclass>(env->NewGlobalRef(local_class));
    env->DeleteLocalRef(local_class);

    return new XConfigClassHolder { .class_holder = global_class, .constructor_id = constructor_id };
}

static XConfigClassHolder* loadXConfigClassByStaticFactory(JNIEnv* env, const char* name, const char* method, const char* source) {
    jclass local_class = env->FindClass(name);
    if (local_class == NULL) {
        return NULL;
    }
    std::string signature = std::string("(L") + source + ";)L" + name + ";";
    jmethodID constructor_id = env->GetStaticMethodID(local_class, method, signature.c_str());
    if (constructor_id == NULL) {
        return NULL;
    }
    jclass global_class = reinterpret_cast<jclass>(env->NewGlobalRef(local_class));
    env->DeleteLocalRef(local_class);

    return new XConfigClassHolder { .class_holder = global_class, .constructor_id = constructor_id };
}

static CollectionHolder* loadCollectionHolder
(JNIEnv* env, const char* name, const char* constructor_signature, const char* add_method, const char* add_signature) {
    jclass local_class = env->FindClass(name);
    if (local_class == NULL) {
        return NULL;
    }
    jmethodID constructor_id = env->GetMethodID(local_class, "<init>", constructor_signature);
    if (constructor_id == NULL) {
        return NULL;
    }
    jmethodID add_id =  env->GetMethodID(local_class, add_method, add_signature);
    if (add_id == NULL) {
        return NULL;
    }
    jclass global_class = reinterpret_cast<jclass>(env->NewGlobalRef(local_class));
    env->DeleteLocalRef(local_class);

    return new CollectionHolder { .class_holder = global_class, .constructor_id = constructor_id, .add_id = add_id };
}

static FieldHolder* loadFieldHolder(JNIEnv* env, const char* owner, const char* name, const char* type) {
    jclass local_class = env->FindClass(owner);
    if (local_class == NULL) {
        return NULL;
    }
    jfieldID field_id = env->GetFieldID(local_class, name, type);
    if (field_id == NULL) {
        return NULL;
    }
    jclass global_class = reinterpret_cast<jclass>(env->NewGlobalRef(local_class));
    env->DeleteLocalRef(local_class);

    return new FieldHolder { .class_holder = global_class, .field_id = field_id };
}

static inline jobject safe_CallStaticObjectMethod(JNIEnv* env, jclass owner_class, jmethodID methodId, ...) {
    va_list args;
    va_start(args, methodId);
    jobject result = env->CallStaticObjectMethodV(owner_class, methodId, args);
    va_end(args);
    if (env->ExceptionCheck() == JNI_TRUE) {
        result = NULL;
    }
    return result;
}

#endif