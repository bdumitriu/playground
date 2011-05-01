package gw.storage.csvn.bindings;

import java.util.List;
import java.util.Map;
import java.util.SortedSet;

/**
 * SVNJNIAdapter provides the RA layer JNI interface. 
 */
public class SVNJNIAdapter
{
    /**
     * this revision number can be used for getting the head revision
     */
    public final static int HEAD_REVISION = -1;

    /**
     * there should be a wrapper class to set the sessionPointer
     */
    public long _sessionPointer = 0;

    /**
     * A sessionPointer is the actual C pointer.
     *
     * @param sessionPointer represents C pointer to session data.
     */
    public void setSessionPointer(long sessionPointer) {
        _sessionPointer = sessionPointer;
    }
    
    static
    {
        System.loadLibrary("svnra");
    }
    
    /**
     * Constructor opens a new RA layer session.
     *
     * @param username 
     * @param password
     * @param repoURL URL or path to repository
     */    
    public SVNJNIAdapter(String username, String password, String repoURL) throws RAException
    {
        openSession(username, password, repoURL);
    }
        
    /**
     * Open a new RA layer session.
     *
     * @param username 
     * @param password
     * @param repoURL URL or path to repository
     */    
    private synchronized native void openSession(String username, 
                                    String password, 
                                    String repoUrl) throws RAException;
    
    /**
     * Close a RA layer session.
     *
     * @param sessionPointer represents C pointer to session data.
     */    
    public synchronized native void closeSession(long sessionPointer);

    /**
     * Get the current session's revision number.
     *
     * @param sessionPointer represents C pointer to session data.
     */
    public synchronized native long getCurrentRevision(long sessionPointer);

    /**
     * Select the revision number to use as base for further RA calls.
     *
     * @param revision the revision number to select.
     * @param sessionPointer represents C pointer to session data.
     */
    public synchronized native void selectRevision(long revision, long sessionPointer);

    /**
     * Update to the newest available revision.
     *
     * @param sessionPointer represents C pointer to session data.
     */
    public void update(long sessionPointer)
    {
        this.selectRevision(HEAD_REVISION, sessionPointer);
    }
    
    /**
     * Get a dir listing.
     *
     * @param path path to get the dirlisting from
     * @param revision The revision to get the dirlisting from
     * @param recurse Recurse on subdirectories
     * @param sessionPointer represents C pointer to session data
     * @return List of strings
     */  
    public synchronized native List getDirListing(String path, 
                                     long revision, 
                                     boolean recurse, 
                                     long sessionPointer) throws RAException;

    /**
     * Check if a file exists in the head revision.
     *
     * @param path path to check
     * @param sessionPointer represents C pointer to session data
     * @return true when the file exists
     */ 
    public synchronized native boolean fileExists(String path, 
                                     long sessionPointer) throws RAException;
    
    /**
     * Check if a path is a directory in the head revision.
     *
     * @param path path to check
     * @param sessionPointer represents C pointer to session data
     * @return true when the path is a directory
     */ 
    public synchronized native boolean isDirectory(String path,  
                                      long sessionPointer) throws RAException;
    
    /**
     * Get a file from the repository.
     *
     * @param path file to get data from
     * @param revision 
     * @param sessionPointer represents C pointer to session data
     * @return all bytes from the file
     * @throws RAException when the file does not exists.
     */     
    public synchronized native byte[] getFile(String path, 
                                 long revision, 
                                 long sessionPointer) throws RAException;
    
    /**
     * Get a log from a path of the last revision.
     *
     * @param path file to get the log of
     * @param sessionPointer represents C pointer to session data
     * @return a SortedSet containing RALogMessage objects
     */     
    public synchronized native SortedSet getLog(String path, 
                                   long sessionPointer) throws RAException;
     
    /**
     * Get a blame for each line of a file.
     *
     * @param path file to get the blame of
     * @param startRevision the revision to start finding blame info from
     * @param endRevision the revision to end with finding blame info
     * @param sessionPointer represents C pointer to session data
     * @return a List containing StorageBlameLine objects
     */     
    public synchronized native List blame(String path, 
                                   long startRevision,
                                   long endRevision,
                                   long sessionPointer) throws RAException;

    /**
     * Get the properties of a path or directory.
     *
     * @param path path to get the properties from
     * @param revision
     * @param sessionPointer represents C pointer to session data
     * @return a map containing key,value pairs
     */
    public synchronized native Map getProperties(String path, 
                                    long revision, 
                                    long sessionPointer) throws RAException;
    
    /**
     * Start an RA layer transaction.
     *
     * @param logMessage 
     * @param sessionPointer represents C pointer to session data
     */
    public synchronized native void transactionStart(String logMessage, 
                                        long sessionPointer) throws RAException;
    
    /**
     * Abort an RA layer transaction.
     *
     * @param sessionPointer represents C pointer to session data
     */    
    public synchronized native void transactionAbort(long sessionPointer) throws RAException;
    
    /**
     * Commit an RA layer transaction.
     *
     * @param sessionPointer represents C pointer to session data
     */       
    public synchronized native void transactionCommit(long sessionPointer) throws RAException;
    
    /**
     * Send a file to the RA layer.
     *
     * @param pathname source file to send
     * @param sourcefilename the sourcefile to read the data from
     * @param fileExists are you sending a new file or just updating?
     * @param sessionPointer represents C pointer to session data
     */    
    public synchronized native void transactionSendFile(String pathname, 
                                           String sourcefilename,
                                           boolean fileExists, 
                                           long sessionPointer) throws RAException;
    
    /**
     * Copy a file on the RA layer.
     *
     * @param pathname destination path
     * @param sourcePath source path
     * @param sessionPointer represents C pointer to session data
     */        
    public synchronized native void transactionCopyFile(String pathname, 
                                           String sourcePath, 
                                           long sessionPointer) throws RAException;
    
    /**
     * Enter a repository directory.
     *
     * @param dirname directory name to enter
     * @param sessionPointer represents C pointer to session data
     */        
    public synchronized native void transactionEnterDirectory(String dirname, 
                                                 long sessionPointer) throws RAException;
    
    /**
     * Leave a repository directory.
     *
     */        
    public synchronized native void transactionLeaveDirectory(long sessionPointer) throws RAException;
    
    /**
     * Change a property of a file.
     *
     * @param pathname file to change the property for.
     * @param name key to change
     * @param value value
     * @param sessionPointer represents C pointer to session data
     */ 
    public synchronized native void transactionChangeFileProperty(String pathname, 
                                                     String name, 
                                                     String value, 
                                                     long sessionPointer) throws RAException;
    
    /**
     * Change a property of a directory.
     *
     * @param pathname directory to change the property for.
     * @param name key to change
     * @param value value
     * @param sessionPointer represents C pointer to session data
     */     
    public synchronized native void transactionChangeDirProperty(String name, 
                                                    String value, 
                                                    long sessionPointer) throws RAException;
    

    /**
     * Add a new repository directory.
     *
     * @param dirname directory to add.
     * @param sessionPointer represents C pointer to session data
     */       
    public synchronized native void transactionAddDirectory(String dirname, 
                                                      long sessionPointer) throws RAException;
    
    /**
     * Remove a repository entry. This can be a file or a directory.
     *
     * @param entry entry to remove
     * @param sessionPointer represents C pointer to session data
     */     
    public synchronized native void transactionRemoveEntry(String entry, 
                                              long sessionPointer) throws RAException;
}
