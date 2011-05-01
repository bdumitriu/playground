#include <jni.h>
#include <stdio.h>

#include "svn_pools.h"
#include "svn_config.h"
#include "svn_opt.h"
#include "svn_ra.h"
#include "svn_client.h"

#include "Subversion.h"


/* !!! Pointers to the current RA session etc.  These shouldn't be
   global (obviously) but should be stored in the Java-side object. */
void * ra_baton;
svn_ra_plugin_t * ra_lib;
void * ra_session;
apr_pool_t * pool;
svn_ra_callbacks_t callbacks;


static void barf(svn_error_t * err)
{
    svn_handle_error(err, stderr, TRUE);
}


JNIEXPORT void JNICALL 
Java_Subversion_openSession(JNIEnv * env, jobject obj, jstring repoURL)
{
    svn_error_t * err;

    const char * repoURLC = (*env)->GetStringUTFChars(env, repoURL, 0);

    apr_initialize();

    pool = svn_pool_create(0);
    if (!pool) abort();

    err = svn_config_ensure(0, pool);
    if (err) barf(err);

    apr_hash_t * config;
    err = svn_config_get_config(&config, 0, pool);
    if (err) barf(err);

    /* Set up authentication info from the config files (copied from
       subversion/clients/cmdline/main.c). */
    apr_array_header_t * providers =
        apr_array_make(pool, 10, sizeof (svn_auth_provider_object_t *));

    svn_auth_provider_object_t * provider;
    svn_client_get_simple_provider(&provider, pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_username_provider(&provider, pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_server_trust_file_provider(&provider, pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_client_cert_file_provider(&provider, pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_client_cert_pw_file_provider(&provider, pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;

    /* Get the plugin that handles the protocol for `repoURL'. */
    err = svn_ra_init_ra_libs(&ra_baton, pool);
    if (err) barf(err);

    err = svn_ra_get_ra_library(&ra_lib, ra_baton, repoURLC, pool);
    if (err) barf(err);

    memset(&callbacks, 0, sizeof callbacks);
    svn_auth_open(&callbacks.auth_baton, providers, pool);

    /* Open a session to `repoURL'. */
    err = ra_lib->open(&ra_session, repoURLC, &callbacks, 0, config, pool);
    if (err) barf(err);

    (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
}


JNIEXPORT void JNICALL 
Java_Subversion_readDirectory(JNIEnv * env, jobject obj,
    jstring path)
{
    svn_error_t * err;

    const char * pathC = (*env)->GetStringUTFChars(env, path, 0);

    /* Get the head revision. */
    svn_revnum_t rev;
    err = ra_lib->get_latest_revnum(ra_session, &rev, pool);
    if (err) barf(err);

    /* Get a list of directory entries at `repoURL'. */
    apr_hash_t * dirents;
    err = ra_lib->get_dir(ra_session, pathC, rev, &dirents, 0, 0, pool);
    if (err) barf(err);
 
    /* Inefficient: should be initialised once (from a static
       method). */
    jclass cls = (*env)->GetObjectClass(env, obj);
    jmethodID mid = (*env)->GetMethodID(env, cls, "acceptDirEntry", "(Ljava/lang/String;)V");
    if (!mid) abort();
    
    /* Send the list of entries to the Java side. */
    apr_hash_index_t *hi;
    for (hi = apr_hash_first(pool, dirents); hi; hi = apr_hash_next (hi)) {
        const char * key;
        apr_hash_this (hi, (const void * *) &key, 0, 0);

        (*env)->CallVoidMethod(env, obj, mid,
            (*env)->NewStringUTF(env, key));
    }

    (*env)->ReleaseStringUTFChars(env, path, pathC);
}
