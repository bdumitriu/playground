#include <jni.h>
#include <stdio.h>

#include <svn_pools.h>
#include <svn_config.h>
#include <svn_time.h>
#include <svn_opt.h>
#include <svn_path.h>
#include <svn_repos.h>
#include <svn_ra.h>
#include <svn_client.h>

#include "SVNJNIAdapter.h"
#include "java_classes.h"
#include "stack.h"
#include "blame.h"


// Structure containing session-related data.
// A per-session pointer to this data is stored by the Java code
struct session_data {

    void* ra_baton;                 // The general RA plugin
    svn_ra_plugin_t* ra_lib;        // The concrete RA library
    void* ra_session;               // The current RA session
    apr_pool_t* pool;               // The memory pool for this session
    svn_ra_callbacks_t* callbacks;  // The callbacks required by the RA layer

    // Authentication credentials
    char* username;
    char* password;

    // Transaction related batons and stuff
    void* root_baton;                   // The repository root baton
    svn_revnum_t base_revision;         // The revision used as base for this session
    const svn_delta_editor_t* editor;   // The editor instance for committing
    void* edit_baton;                   // Baton for the above editor
    struct stack_elem* dir_batons;      // A pointer to the top of the directory baton stack
};


/* Java baton is used for passing the java environment and some java object you
   are working with, to a c callback function. 
*/
struct java_baton 
{
    JNIEnv      *env;
    jobject     obj;
    apr_pool_t  *pool;
};


// Opens a temporary file; RA callback function required for commits
static svn_error_t* open_tmp_file(apr_file_t** fp, void* callback_baton, apr_pool_t* pool) {
    const char* unique_name;
    svn_error_t* err = svn_io_open_unique_file(fp, &unique_name, "/tmp/gw_svn_commit", ".tmp", 
                                               TRUE, pool);
    return err;
}

// Callback used by svn authentication to retrieve username and password
static svn_error_t* password_prompt(svn_auth_cred_simple_t** cred, void* baton, const char* realm, 
                                    const char* username, svn_boolean_t may_save, apr_pool_t* pool){
    
    svn_auth_cred_simple_t* retval = (svn_auth_cred_simple_t*) apr_pcalloc(pool, sizeof(*retval));
    // We set the baton to the session when registering this callback, so we can safely cast back
    struct session_data* session = (struct session_data*) baton;
    retval->username = apr_pstrdup(pool, session->username);
    retval->password = apr_pstrdup(pool, session->password);
    retval->may_save = may_save;

    *cred = retval;
    return SVN_NO_ERROR;
}

/* open new RA session
*/
JNIEXPORT void JNICALL 
Java_gw_storage_bindings_SVNJNIAdapter_openSession(JNIEnv * env, jobject obj, jstring java_username, 
                                                   jstring java_password, jstring repoURL)
{
    // Init the session data structure
    struct session_data* session = calloc(1, sizeof(struct session_data));

    // Grab username and password from the Java side, and throw them into the session
    const char* username = (*env)->GetStringUTFChars(env, java_username, 0);
    const char* password = (*env)->GetStringUTFChars(env, java_password, 0);

    session->username = strdup(username);
    session->password = strdup(password);
        
    (*env)->ReleaseStringUTFChars(env, java_username, username);
    (*env)->ReleaseStringUTFChars(env, java_password, password);
        
    const char * repoURLC = (*env)->GetStringUTFChars(env, repoURL, 0);
    svn_error_t * err;

    apr_initialize();

    // create new pool
    session->pool = svn_pool_create(0);
    if (!session->pool)
    {
        abort();
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;        
    }

    err = svn_config_ensure(0, session->pool);
    if (err)
    {
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;
    }
    
    apr_hash_t * config;
    err = svn_config_get_config(&config, 0, session->pool);
    if (err)
    {
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;
    }

    /* Set up authentication info from the config files (copied from
       subversion/clients/cmdline/main.c). */
    apr_array_header_t * providers =
        apr_array_make(session->pool, 10, sizeof (svn_auth_provider_object_t *));

    svn_auth_provider_object_t * provider;
    // Sneakily using the session pointer as baton, allowing the callback to
    // retrieve session username and password
    svn_client_get_simple_prompt_provider(&provider, &password_prompt, (void*)session, 1, 
                                          session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_simple_provider(&provider, session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_username_provider(&provider, session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_server_trust_file_provider(&provider, session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_client_cert_file_provider(&provider, session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;
    svn_client_get_ssl_client_cert_pw_file_provider(&provider, session->pool);
    APR_ARRAY_PUSH(providers, svn_auth_provider_object_t *) = provider;

    /* Get the plugin that handles the protocol for `repoURL'. */
    err = svn_ra_init_ra_libs(&(session->ra_baton), session->pool);
    if (err)
    {
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;
    }

    err = svn_ra_get_ra_library(&(session->ra_lib), session->ra_baton, repoURLC, session->pool);
    if (err)
    {
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;
    }

    session->callbacks = (svn_ra_callbacks_t*) apr_pcalloc(session->pool, 
                                                           sizeof(*(session->callbacks)));
    session->callbacks->open_tmp_file = &open_tmp_file;
    svn_auth_open(&(session->callbacks->auth_baton), providers, session->pool);

    /* Open a session to `repoURL'. */
    err = session->ra_lib->open(&(session->ra_session), repoURLC, session->callbacks, 0, 
                                config, session->pool);
    if (err)
    {
        (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);
        java_barf(env, err);
        return;
    }

    (*env)->ReleaseStringUTFChars(env, repoURL, repoURLC);

    // Stuff the head revision as base in the session
    session->ra_lib->get_latest_revnum(session->ra_session, &(session->base_revision), session->pool);

    // Stuff the batons and other pointers to the Java class
    jclass java_class = (*env)->GetObjectClass(env, obj);
    jmethodID mid = (*env)->GetMethodID(env, java_class, "setSessionPointer", "(J)V");
    if (mid == 0) {
        err =  svn_error_create(SVN_ERR_RA_ILLEGAL_URL, NULL, "method not found");
        java_barf(env,err);
        return;
    }
    (*env)->CallVoidMethod(env, obj, mid, session);
}


/* close RA layer session.
*/
JNIEXPORT void JNICALL 
Java_gw_storage_bindings_SVNJNIAdapter_closeSession(JNIEnv * env, jobject obj, 
                                                    jlong java_session_data)
{
    struct session_data* session = (struct session_data*)(long) java_session_data;

    free(session->username);
    free(session->password);


    // delete local refs
    deleteAllJavaClasses(env);

    // close ra session
    svn_pool_destroy(session->pool);
    stack_free(&(session->dir_batons));
    free(session);
    // ra_lib->close(ra_session); does not exist!
    apr_terminate();
}

JNIEXPORT jlong JNICALL Java_gw_storage_bindings_SVNJNIAdapter_getCurrentRevision
  (JNIEnv* env, jobject obj, jlong java_session_data) {
      
    struct session_data* session = (struct session_data*) (long)java_session_data;

    return (jlong) session->base_revision;
}

JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_selectRevision
  (JNIEnv* env, jobject obj, jlong revnum, jlong java_session_data) {
     
    struct session_data* session = (struct session_data*) (long)java_session_data;

    svn_error_t* err;
    // Get latest revision number
    svn_revnum_t latest_revnum;
    err = session->ra_lib->get_latest_revnum(session->ra_session, &latest_revnum, session->pool);
    if (err) java_barf(env, err);

    // Bail out if the requested revision does not exist
    if (revnum > latest_revnum) {
        printf("native: warning: invalid parameter supplied to selectRevision ignored\n");
        return;
    }

    session->base_revision = revnum;
}

/* get node type (return: svn_node_dir, svn_node_file, svn_node_unknown)
   throw RAException when something goes wrong
*/
static svn_node_kind_t get_node_kind(JNIEnv *env, jstring path, jlong java_session_data)
{
    struct session_data* session = (struct session_data*) (long)java_session_data;
    const char *strPath = (*env)->GetStringUTFChars(env, path, 0);  
    svn_error_t *err;
    
    // determine node kind of path
    svn_node_kind_t kind;
    err = session->ra_lib->check_path(session->ra_session, strPath, -1, &kind, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, strPath);  
        java_barf(env, err);                
        return svn_node_unknown;
    }
    
    (*env)->ReleaseStringUTFChars(env, path, strPath);  
    return kind;    
}

/* Check if a file exists in the head revision.
*/
JNIEXPORT jboolean JNICALL Java_gw_storage_bindings_SVNJNIAdapter_fileExists
  (JNIEnv *env, jobject obj, jstring path, jlong java_session_data)
{
    // check if a node is a file
    svn_node_kind_t kind = get_node_kind(env, path, java_session_data);
    return ((kind == svn_node_file) || (kind == svn_node_dir));    
}

/* Check if a path is a directory, using the head revision.
*/
JNIEXPORT jboolean JNICALL Java_gw_storage_bindings_SVNJNIAdapter_isDirectory
  (JNIEnv *env, jobject obj, jstring path, jlong java_session_data)
{
    // check if a node is a directory    
    svn_node_kind_t kind = get_node_kind(env, path, java_session_data);
    return (kind == svn_node_dir);    
}

/* Get all properties of a file or directory.
*/
JNIEXPORT jobject JNICALL
Java_gw_storage_bindings_SVNJNIAdapter_getProperties(JNIEnv * env, jobject obj, jstring path, 
                                                     jlong revision, jlong java_session_data)
{
    struct session_data* session = (struct session_data*) (long)java_session_data;

    jclass cls_map = get_cls_HashMap(env); // used for key,value pair storage
    if (mid_HashMapPut==NULL) return NULL;
    
    const char *str_path = (*env)->GetStringUTFChars(env, path, 0); 
    svn_error_t *err;
    apr_hash_t *props;

    // get the properties from ra layer
    err = session->ra_lib->get_file(session->ra_session, str_path, revision, NULL, NULL, 
                                    &props, session->pool);    
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        java_barf(env, err);
        return NULL;
    }    
   
    // create a new map for property storage
    jobject map = (*env)->NewObject(env, cls_map, mid_HashMap);
    if (map==NULL) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        return NULL;
    }
    
    // get all properties
    apr_hash_index_t *index;
    for (index = apr_hash_first(session->pool, props); index; index = apr_hash_next(index)) 
    {
        const char *key;
        svn_string_t *val;
        apr_hash_this(index, (void*) &key, NULL, (void*) &val);
        
        // add key,value pair to java map
        jstring jkey = (*env)->NewStringUTF(env, key);
        jstring jval = (*env)->NewStringUTF(env, val->data);
        (*env)->CallObjectMethod(env, map, mid_HashMapPut, jkey, jval);
    }   

    (*env)->ReleaseStringUTFChars(env, path, str_path);  
    return map;    
}


/* Get a file from the repository.
*/
JNIEXPORT jbyteArray JNICALL
Java_gw_storage_bindings_SVNJNIAdapter_getFile(JNIEnv * env, jobject obj, jstring path, 
                                               jlong revision, jlong java_session_data)
{
    struct session_data* session = (struct session_data*) (long)java_session_data;

    const char *str_path = (*env)->GetStringUTFChars(env, path, 0); 
    svn_error_t *err;
    char *buffer;
    apr_size_t buffer_len;
    
    buffer = (char*) malloc(sizeof(char)); // buffer used for storage of file data
    buffer_len = 0;
        
    // nested stream_write_handler, used by ra_lib->get_file
    svn_error_t *write_handler(void *baton, const char *new_data, apr_size_t *len)
    {
        // append new_data to buffer
        char *tmp = (char*) malloc((buffer_len + *len + 1) * sizeof(char));
        memcpy(tmp, buffer, buffer_len);
        memcpy(tmp + buffer_len, new_data, *len);
        free(buffer);
        buffer = tmp;
        buffer_len += *len;

        return SVN_NO_ERROR;
    }

    // create stream
    svn_stream_t *stream = svn_stream_create(session->ra_baton, session->pool);
    svn_stream_set_write(stream, write_handler);
        
    // get file from repository
    err = session->ra_lib->get_file(session->ra_session, str_path, revision, stream, 
                                    NULL, NULL, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        free(buffer);
        java_barf(env, err);
        return NULL;
    }
    
    // close stream
    err = svn_stream_close(stream); 
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        free(buffer);
        java_barf(env, err);        
        return NULL;
    }
    
    (*env)->ReleaseStringUTFChars(env, path, str_path);  
    
    // return byte[]
    buffer[buffer_len] = '\0';
    jbyteArray java_array = (*env)->NewByteArray(env, buffer_len);
    (*env)->SetByteArrayRegion(env, java_array, 0, buffer_len, buffer);

    free(buffer);
    
    return java_array;
}


static svn_error_t* commit_callback(svn_revnum_t new_revision, const char* date, 
                                    const char* author, void* baton) {
    printf("== Commit callback ==\n");
    printf("* New revision:  %d\n", (int)new_revision);
    printf("* Commit date:   %s\n", date);
    printf("* Commit author: %s\n", author);

    return SVN_NO_ERROR;
}

/* Start a RA transaction (get a commit_editor)
*/
JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionStart (JNIEnv *env, 
                                                                         jobject obj, 
                                                                         jstring java_log_message, 
                                                                         jlong java_session_data) {
    struct session_data* session = (struct session_data*) (long)java_session_data;

    // Convert parameters to C
    const char* log_message = (*env)->GetStringUTFChars(env, java_log_message, 0);
    
    svn_error_t* err;
    err = session->ra_lib->get_latest_revnum(session->ra_session, &(session->base_revision), 
                                             session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_log_message, log_message);  
        java_barf(env,err);
        return;
    }
    
    // Allocate a callback baton for the commit callback
    void* cb_baton = (void*) apr_pcalloc(session->pool, 1);
   
    // Get a commit editor from the RA layer
    err = session->ra_lib->get_commit_editor(session->ra_session, &(session->editor), 
                                             &(session->edit_baton), log_message, &commit_callback, 
                                             cb_baton, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_log_message, log_message);  
        java_barf(env,err);
        return;
    }

    // Open the root directory of the repository
    err = session->editor->open_root(session->edit_baton, session->base_revision, session->pool, 
                                     &(session->root_baton));
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_log_message, log_message);  
        java_barf(env,err);
        return;
    }

    // Copy it to the directory stack
    stack_init(&(session->dir_batons));
    stack_push(&(session->dir_batons), session->root_baton);
    
    (*env)->ReleaseStringUTFChars(env, java_log_message, log_message);  
}

/* abort transaction
*/
JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionAbort (JNIEnv *env, 
                                                            jobject obj, jlong java_session_data) {
    struct session_data* session = (struct session_data*)(long) java_session_data;

    // TODO: this (and transactionCommit) dies horribly is editor is null
    svn_error_t* err;
    err = session->editor->abort_edit(session->edit_baton, session->pool);
    if (err) 
    {
        java_barf(env,err);
        return;
    }

    stack_free(&(session->dir_batons));
}

/* commit a transaction
*/
JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionCommit (JNIEnv *env, 
                                                            jobject obj, jlong java_session_data) {
    struct session_data* session = (struct session_data*) (long)java_session_data;

    svn_error_t* err;
    // Close all open directories, including the root
    while(stack_top(&(session->dir_batons))) {
        err = session->editor->close_directory(stack_pop(&(session->dir_batons)), session->pool);
        if (err) 
        {
            java_barf(env,err);
            return;
        }
    }
    err = session->editor->close_edit(session->edit_baton, session->pool);
    if (err) 
    {
        java_barf(env,err);
        return;
    }
    
    stack_free(&(session->dir_batons));
}

/* send file to repository
*/
svn_error_t* transaction_send_file(const char* pathname, const char* sourcefile, svn_boolean_t exists, 
                                   const char* source_path, struct session_data* session) {
    svn_error_t* err;

    // Tell the editor we're going to add or open a file
    void* file_baton;
    if (exists == FALSE) {
        // TODO: no directory support yet. Java needs to store our pointer first,
        //       after that we can simple store the parent baton there and let Java
        //       do all the directory traversing.
        if (source_path) {
            // Copying; copy the file history from source_path
            err = session->editor->add_file(pathname, stack_top(&(session->dir_batons)), 
                                            source_path, session->base_revision, session->pool, 
                                            &file_baton);
        } else {
            // Creating; empty file history
            err = session->editor->add_file(pathname, stack_top(&(session->dir_batons)), 0, 0, 
                                            session->pool, &file_baton);
        }
    } else {
        // Modifying; keep file history
        err = session->editor->open_file(pathname, stack_top(&(session->dir_batons)), 
                                         session->base_revision, session->pool, &file_baton);
    }
    
    if (err) return err;

    // Get the delta handler for the new file (no checksumming)
    void* handler_baton;
    svn_txdelta_window_handler_t handler;
    err = session->editor->apply_textdelta(file_baton, 0, session->pool, &handler, &handler_baton);
    if (err) return err;
    
    // Open the local file
    apr_file_t* file;
    err = svn_io_file_open(&file, sourcefile, APR_READ, APR_OS_DEFAULT, session->pool);
    if (err) return err;

    // Get the stream for the local file, and send it to subversion (again, no checksumming)
    svn_stream_t* filestream;
    filestream = svn_stream_from_aprfile(file, session->pool);
    
    err = svn_txdelta_send_stream(filestream, handler, handler_baton, 0, session->pool);
    if (err) return err;

    // Cleanup: close the file
    err = session->editor->close_file(file_baton, 0, session->pool);
    if (err) return err;
    
    err = svn_stream_close(filestream);
    if (err) return err;

    return err = svn_io_file_close(file, session->pool);
}


JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionSendFile (JNIEnv *env, 
                                                                          jobject obj, 
                                                                          jstring java_pathname, 
                                                                          jstring java_sourcename,
                                                                          jboolean java_exists, 
                                                                          jlong java_session_data){
    struct session_data* session = (struct session_data*)(long) java_session_data;

    // TODO: maybe use APR subpools to prevent memory leaks?
     
    // Convert parameters to C
    const char* pathname = (*env)->GetStringUTFChars(env, java_pathname, 0);
    const char* sourcename = (*env)->GetStringUTFChars(env, java_sourcename, 0);
    svn_boolean_t exists = (java_exists == JNI_TRUE);

    svn_error_t *err = transaction_send_file(pathname, sourcename, exists, 0, session);
    (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);  
    (*env)->ReleaseStringUTFChars(env, java_sourcename, sourcename);  
    if (err)
    {
        java_barf(env,err);
        return;
    }
}

JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionCopyFile (JNIEnv *env, 
                                                                         jobject obj, 
                                                                         jstring java_pathname, 
                                                                         jstring java_sourcepath, 
                                                                         jlong java_session_data) {
    struct session_data* session = (struct session_data*)(long) java_session_data;

    const char* pathname = (*env)->GetStringUTFChars(env, java_pathname, 0);
    const char* sourcepath = (*env)->GetStringUTFChars(env, java_sourcepath, 0);

    // TODO: create a temp file that contains the file sourcepath
    // transaction_send_file needs this tempfile!!!

    svn_error_t *err = transaction_send_file(pathname, "tmp", FALSE, sourcepath, session);

    (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);  
    (*env)->ReleaseStringUTFChars(env, java_sourcepath, sourcepath);

    if (err)
    {
        java_barf(env,err);
        return;
    }    
}

JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionEnterDirectory
  (JNIEnv* env, jobject obj, jstring java_dirname, jlong java_session_data) {
    
    // Copy the session pointer and directory name
    struct session_data* session = (struct session_data*)(long) java_session_data;
    const char* dirname = (*env)->GetStringUTFChars(env, java_dirname, 0);

    // Enter the directory
    void* child_baton;
    svn_error_t* err = session->editor->open_directory(dirname, stack_top(&(session->dir_batons)), 
                                                       session->base_revision, session->pool, 
                                                       &child_baton);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_dirname, dirname);  
        java_barf(env,err);
        return;
    }

    // Put the directory baton on top of the directory stack
    stack_push(&(session->dir_batons), child_baton);
   
    // Release directory name
    (*env)->ReleaseStringUTFChars(env, java_dirname, dirname);  
}


JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionAddDirectory
  (JNIEnv* env, jobject obj, jstring java_dirname, jlong java_session_data) {
    
    // Copy the session pointer and directory name
    struct session_data* session = (struct session_data*)(long) java_session_data;
    const char* dirname = (*env)->GetStringUTFChars(env, java_dirname, 0);

    // Add the directory
    void* child_baton;
    svn_error_t* err = session->editor->add_directory(dirname, stack_top(&(session->dir_batons)), 
                                                      NULL, SVN_INVALID_REVNUM, session->pool,
                                                      &child_baton);

    (*env)->ReleaseStringUTFChars(env, java_dirname, dirname);  

    if (err) 
    {
        java_barf(env, err);
        return;
    }
}


JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionRemoveEntry
  (JNIEnv* env, jobject obj, jstring java_entryname, jlong java_session_data) {
    
    // Copy the session pointer and entry name
    struct session_data* session = (struct session_data*)(long) java_session_data;
    const char* entryname = (*env)->GetStringUTFChars(env, java_entryname, 0);

    // Add the directory
    svn_error_t* err = session->editor->delete_entry(entryname, session->base_revision, 
                                                     stack_top(&(session->dir_batons)), 
                                                     session->pool);

    (*env)->ReleaseStringUTFChars(env, java_entryname, entryname);  

    if (err) 
    {
        java_barf(env, err);
        return;
    }
}


JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionLeaveDirectory
  (JNIEnv* env, jobject obj, jlong java_session_data) {
      
    struct session_data* session = (struct session_data*)(long)java_session_data;
    svn_error_t* err = session->editor->close_directory(stack_pop(&(session->dir_batons)), 
                                                        session->pool);
    if (err) java_barf(env,err);
}

JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionChangeFileProperty
  (JNIEnv* env, jobject obj, jstring java_pathname, jstring java_propname, jstring java_propvalue, 
   jlong java_session_data) {

    // Convert java parameters
    struct session_data* session = (struct session_data*)(long)java_session_data;
    const char* pathname = (*env)->GetStringUTFChars(env, java_pathname, 0);
    const char* propname = (*env)->GetStringUTFChars(env, java_propname, 0);
    const char* propvalue = (*env)->GetStringUTFChars(env, java_propvalue, 0);

    // Open the file
    void* file_baton;
    svn_error_t* err;
    err = session->editor->open_file(pathname, stack_top(&(session->dir_batons)), 
                                     session->base_revision, session->pool, &file_baton);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);
        (*env)->ReleaseStringUTFChars(env, java_propname, propname);
        (*env)->ReleaseStringUTFChars(env, java_propvalue, propvalue);        
        java_barf(env,err);
        return;
    }
    
    // Create an svn_string_t from the propvalue, and apply it
    svn_string_t str_propvalue;
    str_propvalue.data = strdup(propvalue);
    str_propvalue.len = strlen(propvalue);
    err = session->editor->change_file_prop(file_baton, propname, &str_propvalue, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);
        (*env)->ReleaseStringUTFChars(env, java_propname, propname);
        (*env)->ReleaseStringUTFChars(env, java_propvalue, propvalue);        
        java_barf(env,err);
        return;
    }

    // Close the file
    err = session->editor->close_file(file_baton, 0, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);
        (*env)->ReleaseStringUTFChars(env, java_propname, propname);
        (*env)->ReleaseStringUTFChars(env, java_propvalue, propvalue);        
        java_barf(env,err);
        return;
    }

    // Release pathname, propname, propvalue
    (*env)->ReleaseStringUTFChars(env, java_pathname, pathname);
    (*env)->ReleaseStringUTFChars(env, java_propname, propname);
    (*env)->ReleaseStringUTFChars(env, java_propvalue, propvalue);
}


JNIEXPORT void JNICALL Java_gw_storage_bindings_SVNJNIAdapter_transactionChangeDirProperty
  (JNIEnv *env, jobject obj, jstring java_propname, jstring java_propvalue, jlong java_session_data) 
{
    // Convert java parameters
    struct session_data* session = (struct session_data*)(long)java_session_data;
    const char* propname = (*env)->GetStringUTFChars(env, java_propname, 0);
    const char* propvalue = (*env)->GetStringUTFChars(env, java_propvalue, 0);

    // Create an svn_string_t from the propvalue, and apply it
    svn_error_t* err;
    svn_string_t* str_propvalue = (svn_string_t*) apr_pcalloc(session->pool, sizeof(svn_string_t));
    str_propvalue->data = strdup(propvalue);
    str_propvalue->len = strlen(propvalue);
    err = session->editor->change_dir_prop(stack_top(&(session->dir_batons)), propname, 
                                           str_propvalue, session->pool);

    // Release propname, propvalue
    (*env)->ReleaseStringUTFChars(env, java_propname, propname);
    (*env)->ReleaseStringUTFChars(env, java_propvalue, propvalue);
    if (err) java_barf(env,err);
}
  

/* get_dir_listing function does the actual work with the RA layer, that is: getting the 
   dir entries.
*/
svn_error_t *get_dir_listing(const char *dir, svn_revnum_t revision, svn_boolean_t recurse,
                                       struct session_data *session, struct java_baton *jb)
{
    JNIEnv *env = jb->env;
    jobject list = jb->obj;
    svn_error_t * err;
    svn_dirent_t *dirent;

    // get dir entries
    apr_hash_t * dirents;

    err = session->ra_lib->get_dir(session->ra_session, dir, revision, &dirents, 
                                   0, 0, session->pool);
    if (err) return err;

    // add list entries to java list.
    svn_boolean_t first_entry = FALSE;
    apr_hash_index_t *hi;
    for (hi = apr_hash_first(session->pool, dirents); hi; hi = apr_hash_next(hi))
    {
        if (!first_entry) {
            first_entry = TRUE;
            continue;
        }
        const char *path;
        const char *key;
        void *val;
        apr_hash_this(hi, (const void **) &key, NULL, &val);
        dirent = val;

        // join file/dir name with source path name
        path = svn_path_join(dir, key, session->pool);
       
        // only recurse when node is a directory
        if (recurse && dirent->kind == svn_node_dir)
            err = get_dir_listing(path, revision, recurse, session, jb);

        // add entry to list
        if (!err) {
            (*env)->CallBooleanMethod(env, list, mid_ArrayListAdd, 
                                      (*env)->NewStringUTF(env, path));
        }
    }

    return SVN_NO_ERROR;
}

/* get a dirlisting from a path and put the results in a java List
*/
JNIEXPORT jobject JNICALL 
Java_gw_storage_bindings_SVNJNIAdapter_getDirListing(JNIEnv * env, jobject obj, 
                                                     jstring path, jlong revision, 
                                                     jboolean recurse, 
                                                     jlong java_session_data)
{
    struct session_data* session = (struct session_data*) (long)java_session_data;

    svn_error_t * err;
    jclass cls_list = get_cls_arraylist(env);
    if (mid_ArrayListAdd == NULL) return NULL;
    struct java_baton jb;
    svn_revnum_t rev = revision;
    const char * str_path = (*env)->GetStringUTFChars(env, path, 0);
    
    // create a java list to store the entries
    jobject list = (*env)->NewObject(env, cls_list, mid_ArrayList);
    if (list==NULL) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        return NULL;
    }
    
    // add the list to a java-baton, so the list can be used in the get_dir_listing
    // function
    jb.env = env;
    jb.obj = list;
    err = get_dir_listing(str_path, rev, recurse, session, &jb);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        java_barf(env, err);
        return NULL;
    }
    
    (*env)->ReleaseStringUTFChars(env, path, str_path);
    return list;
}


// callback function for getLog
svn_error_t* log_receiver(void *baton, apr_hash_t *changed_paths, 
                                svn_revnum_t revision, const char *author, 
                                const char *date, const char *message, 
                                apr_pool_t *pool)
{
    struct java_baton *jb = baton;
    jobject tree = jb->obj;
    JNIEnv *env = jb->env;
    
    // get java classes
    jclass cls_logmsg = get_cls_RALogMessage(env);
    jclass cls_hashmap = get_cls_HashMap(env);
    jclass cls_date = get_cls_Date(env);
    if (cls_logmsg == NULL) return NULL;
    
    // convert log-data to java
    apr_time_t when;
    jobject jdate = NULL;
    jlong   jrev     = revision;
    jstring jauthor  = (*env)->NewStringUTF(env, author);
    jstring jmessage = (*env)->NewStringUTF(env, message);
        
    /* If date is neither null nor the empty string, it was generated by 
       svn_time_to_string()
       and can be converted to apr_time_t with svn_time_from_string(). */
    if ((date != NULL) && (date != "") && (cls_date != NULL))
    {
        svn_error_t *err;
        err = svn_time_from_cstring (&when, date, jb->pool);
        if (err) 
        {
            java_barf(env, err);
            return NULL;
        }
        
        jdate = (*env)->NewObject(env, cls_date, mid_Date, when);
    }
          
    // get all changed_paths and put them in a java hashmap 
    jobject map = NULL;
    if (cls_hashmap != NULL)
    {
        // create new hashmap
        map = (*env)->NewObject(env, cls_hashmap, mid_HashMap);
        
        apr_hash_index_t *hi;
        for (hi = apr_hash_first(pool, changed_paths); hi; hi = apr_hash_next(hi)) 
        {
            const char * key;
            svn_repos_node_t *node;
            apr_hash_this(hi, (void*) &key, 0, (void*)&node);
                        
            // node->action is a char, so convert it to a string
            char str[2];
            str[0] = node->action;    
            str[1] = '\0';
            
            // add key,value pair to java hashmap
            jstring jval = (*env)->NewStringUTF(env, str);
            jstring jkey = (*env)->NewStringUTF(env, key);
            (*env)->CallObjectMethod(env, map, mid_HashMapPut, jkey, jval);
        }    
    }
    
    // create new RALogMessage and add it to TreeSet
    jobject logmsg = (*env)->NewObject(env, cls_logmsg, mid_RALogMessage, 
                                       jauthor, map, jdate, jmessage, jrev);
    if (logmsg == NULL) 
    {
        return NULL;
    }
    
    (*env)->CallBooleanMethod(env, tree, mid_TreeSetAdd, logmsg);
    return SVN_NO_ERROR;
}


JNIEXPORT jobject JNICALL 
Java_gw_storage_bindings_SVNJNIAdapter_getLog(JNIEnv * env, jobject obj, 
                                              jstring path, jlong java_session_data)
{
    struct session_data* session = (struct session_data*)(long)java_session_data;
    const char *str_path = (*env)->GetStringUTFChars(env, path, 0); 
    svn_error_t *err;
    apr_array_header_t *paths;
    struct java_baton jb;
    
    paths = apr_array_make(session->pool, 1, sizeof(const char*));
    APR_ARRAY_PUSH (paths, const char*) = str_path;    
    
    // Create TreeSet and put it in a java_baton, so it can be used
    // in the callback function
    jclass cls_tree = get_cls_TreeSet(env);
    jobject tree = (*env)->NewObject(env, cls_tree, mid_TreeSet);
    jb.env = env;
    jb.obj = tree;
    jb.pool = session->pool;
   
    if (tree == NULL)
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        return NULL;
    }

    // get log, log_receiver is the callback function used
    err = session->ra_lib->get_log(session->ra_session, paths, 0, session->base_revision, TRUE, FALSE, 
                                   log_receiver, &jb, session->pool);
    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        java_barf(env, err);
        return NULL;
    }        

    //apr_table_clear(paths);
    (*env)->ReleaseStringUTFChars(env, path, str_path);
    return tree;
}


/* Blame callback function */
svn_error_t *blame_receiver(void *baton, 
                           apr_int64_t line_no,
                           svn_revnum_t revision,
                           const char *author,
                           const char *date,
                           const char *line,
                           apr_pool_t *pool)
{
    struct java_baton *jb = baton;
    jobject list = jb->obj;
    JNIEnv *env = jb->env;
    
    // NOTE: If there is no blame information for this line, @a revision will be
    // nvalid and @a author and @a date will be NULL.

    // create blame object
    jclass cls_blame = get_cls_StorageBlameLine(env);
    jstring jauthor = (*env)->NewStringUTF(env, author);
    jstring jline = (*env)->NewStringUTF(env, line);
    jlong jrevision = (long)revision;

    // convert date
    jclass cls_date = get_cls_Date(env);
    jobject jdate = NULL;

    /* If date is neither null nor the empty string, it was generated by 
       svn_time_to_string()
       and can be converted to apr_time_t with svn_time_from_string(). */
    if ((date != NULL) && (date != "") && (cls_date != NULL))
    {
        apr_time_t when;
        svn_error_t *err;
        err = svn_time_from_cstring (&when, date, jb->pool);
        if (err) return err;
        jdate = (*env)->NewObject(env, cls_date, mid_Date, when);
    }

    jobject blame_line = (*env)->NewObject(env, cls_blame, mid_StBlameLine, jdate, jrevision, jauthor, jline);
  
    // add blame object to java list
    (*env)->CallBooleanMethod(env, list, mid_ArrayListAdd, blame_line);
    return SVN_NO_ERROR;
}


JNIEXPORT jobject JNICALL 
Java_gw_storage_bindings_SVNJNIAdapter_blame(JNIEnv * env, jobject obj, 
                                             jstring path, jlong jstartRev, jlong jendRev, 
                                             jlong java_session_data)
{
    struct session_data* session = (struct session_data*)(long)java_session_data;
    const char *str_path = (*env)->GetStringUTFChars(env, path, 0); 
    svn_error_t *err;
    svn_revnum_t start_revnum = jstartRev;
    svn_revnum_t end_revnum = jendRev;
    struct java_baton jb;
    
    // create a java list for storage of the blame line objects
    jclass cls_list = get_cls_arraylist(env);
    if (mid_ArrayListAdd == NULL) return NULL;
    jobject list = (*env)->NewObject(env, cls_list, mid_ArrayList);
    if (list==NULL) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        return NULL;
    }

    // java baton used for blame_receiver (so it knows what java environment is)
    jb.env = env;
    jb.obj = list;
    jb.pool = session->pool;
   
    err = svn_ra_blame(session->ra_lib, session->ra_session, str_path, start_revnum, end_revnum, 
                       blame_receiver, &jb, session->pool);

    if (err) 
    {
        (*env)->ReleaseStringUTFChars(env, path, str_path);
        java_barf(env, err);
        return NULL;
    }        

    (*env)->ReleaseStringUTFChars(env, path, str_path);
    return list;
}
