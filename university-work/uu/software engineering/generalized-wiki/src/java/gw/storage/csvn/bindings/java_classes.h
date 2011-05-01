#include <jni.h>
#include <svn_pools.h>
#include <svn_config.h>
#include <svn_time.h>
#include <svn_opt.h>
#include <svn_path.h>
#include <svn_repos.h>
#include <svn_ra.h>
#include <svn_client.h>


// java methods used
static jmethodID mid_RAException = NULL;  // RAException constructor id
static jmethodID mid_HashMap = NULL;      // HashMap constructor id
static jmethodID mid_HashMapPut = NULL;   // HashMap.put 
static jmethodID mid_ArrayList = NULL;
static jmethodID mid_ArrayListAdd = NULL;
static jmethodID mid_RALogMessage = NULL; // RALogMessage constructor
static jmethodID mid_TreeSet = NULL;      // TreeSet constructor
static jmethodID mid_TreeSetAdd = NULL;
static jmethodID mid_Date = NULL;         // Date constructor (long)
static jmethodID mid_StBlameLine = NULL;  // StorageBlameLine constructor


void deleteAllJavaClasses(JNIEnv *env)
{
    if (mid_RAException) (*env)->DeleteLocalRef(env, mid_RAException);
    if (mid_HashMap) (*env)->DeleteLocalRef(env, mid_HashMap);
    if (mid_HashMapPut) (*env)->DeleteLocalRef(env, mid_HashMapPut);
    if (mid_ArrayList) (*env)->DeleteLocalRef(env, mid_ArrayList);
    if (mid_ArrayListAdd) (*env)->DeleteLocalRef(env, mid_ArrayListAdd);
    if (mid_RALogMessage) (*env)->DeleteLocalRef(env, mid_RALogMessage);
    if (mid_TreeSet) (*env)->DeleteLocalRef(env, mid_TreeSet);
    if (mid_TreeSetAdd) (*env)->DeleteLocalRef(env, mid_TreeSetAdd);        
    if (mid_Date) (*env)->DeleteLocalRef(env, mid_Date);                
    if (mid_StBlameLine) (*env)->DeleteLocalRef(env, mid_StBlameLine);                
}


/* Find RAException class and get constructor id.
*/
static jclass get_cls_RAException(JNIEnv *env)
{
    jclass cls = NULL;    

    if ((cls == NULL) || (mid_RAException == NULL))
    {
        cls = (*env)->FindClass(env, "gw/storage/bindings/RAException");
        mid_RAException = (*env)->GetMethodID(env, cls, "<init>", "(Ljava/lang/String;I)V");
    }
   
    return cls;
}

/* Find RALogMessage class and get constructor id.
*/
static jclass get_cls_RALogMessage(JNIEnv *env)
{
    jclass cls = NULL;    
    
    if ((cls == NULL) || (mid_RALogMessage == NULL))
    {
        cls = (*env)->FindClass(env, "gw/storage/bindings/SVNRAStorageLogMessage");
        mid_RALogMessage = (*env)->GetMethodID(env, cls, "<init>", 
                   "(Ljava/lang/String;Ljava/util/Map;Ljava/util/Date;Ljava/lang/String;J)V");
    }
    
    return cls;
}


/* Find StorageBlameLine class and get constructor id.
*/
static jclass get_cls_StorageBlameLine(JNIEnv *env)
{
    jclass cls = NULL;    
    
    if ((cls == NULL) || (mid_StBlameLine == NULL))
    {
        cls = (*env)->FindClass(env, "gw/storage/StorageBlameLine");
        mid_StBlameLine = (*env)->GetMethodID(env, cls, "<init>", 
                   "(Ljava/util/Date;JLjava/lang/String;Ljava/lang/String;)V");
    }

    return cls;
}

/* Find TreeSet class and get constructor and method id.
*/
static jclass get_cls_TreeSet(JNIEnv *env)
{
    jclass cls = NULL;    

    if ((cls == NULL) || (mid_TreeSet == NULL))
    {
        cls = (*env)->FindClass(env, "java/util/TreeSet");
        mid_TreeSet = (*env)->GetMethodID(env, cls, "<init>", "()V");
        mid_TreeSetAdd = (*env)->GetMethodID(env, cls, "add", "(Ljava/lang/Object;)Z");
    }
    
    return cls;
}

/* Find Date class and get constructor id.
*/
static jclass get_cls_Date(JNIEnv *env)
{
    jclass cls = NULL;    

    if ((cls == NULL) || (mid_Date == NULL))
    {
        cls = (*env)->FindClass(env, "java/util/Date");
        mid_Date = (*env)->GetMethodID(env, cls, "<init>", "(J)V");
    }
    
    return cls;
}

/* Find HashMap class and get constructor and method id.
*/
static jclass get_cls_HashMap(JNIEnv *env)
{
    jclass cls = NULL;
    
    if ((cls == NULL) || (mid_HashMapPut == NULL))
    {
	    cls = (*env)->FindClass(env, "java/util/HashMap");
        if (cls == NULL) return NULL;
  	    mid_HashMap = (*env)->GetMethodID(env, cls, "<init>", "()V");
  	    mid_HashMapPut = (*env)->GetMethodID(env, cls, "put", 
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
    }
    
    return cls;
}


/* Find ArrayList class and get constructor and method id.
*/
static jclass get_cls_arraylist(JNIEnv *env)
{
    jclass cls = NULL;
    
    if ((cls == NULL) || (mid_ArrayListAdd == NULL))
    {
	    cls = (*env)->FindClass(env, "java/util/ArrayList");
        if (cls == NULL) return NULL;
  	    mid_ArrayList = (*env)->GetMethodID(env, cls, "<init>", "()V");
  	    mid_ArrayListAdd = (*env)->GetMethodID(env, cls, "add", "(Ljava/lang/Object;)Z");
    }
    
    return cls;
}


/* Throw RAException to java side.
*/
static void java_barf(JNIEnv *env, svn_error_t *err)
{
    jclass cls_ex = get_cls_RAException(env);
    
    if (mid_RAException == NULL) 
    {
        svn_error_clear(err);
        return;
    }

    // create new RAException and throw it
    jstring msg = (*env)->NewStringUTF(env, err->message);
    jobject exc = (*env)->NewObject(env, cls_ex, mid_RAException, msg, err->apr_err);
    if (exc==NULL) 
    {  
        svn_error_clear(err);
        return;
    }
  
    svn_error_clear(err);
    (*env)->Throw(env, exc);
}
