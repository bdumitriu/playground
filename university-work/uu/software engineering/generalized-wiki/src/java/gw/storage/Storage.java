package gw.storage;

import java.io.*;
import java.util.*;


/**
 * The base interface for the storage system.
 *
 * 
 */
public interface Storage
{

    /**
     * Retrieve the content together with the author, the revision and the date of the last change of each line
     *
     * @param path the name of the file to write to
     * @param revisionStart the first revision to show
     * @param revisionEnd the last revision to show
     * @return Iterator containing StorageBlameLine objects
     */
    public Iterator<StorageBlameLine> blame(String path,
                          long revisionStart,
                          long revisionEnd) throws StorageException;

    /**
     * Returns an OutputStream object to write to a file
     *
     * @param path the name of the file to write to
     * @return the OutputStream object
     * @see OutputStream
     */
    public OutputStream storeFile(String path) throws StorageException;

    /**
     * Returns an InputStream object to read from a file
     *
     * @param path the name of the file to read from
     * @return the InputStream object (empty when file does not exist).
     * @exception StorageException when path is a directory.
     * @see InputStream
     */
    public InputStream getFile(String path) throws StorageException;

    /**
     * Returns an InputStream object to read from a file with a certain revision
     *
     * @param path the name of the file to read from
     * @param revision the specified revision
     * @return the InputStream object
     * @see InputStream
     */
    public InputStream getFile(String path, long revision) throws StorageException;

    /**
     * Returns an Iterator object to iterate over the contents of a directory
     * Is non-recursive by default. Lists from the HEAD revision.
     * For recursive directory listing getDirListing(path, recurse)
     *
     * @param path the pathname of the directory
     * @return the Iterator over the paths contained in the given directory
     * relative to the root
     * @see Iterator
     */
    public Iterator<String> getDirListing(String path) throws StorageException;

    /**
     * Returns an Iterator object to iterate over the contents of a directory
     * Allows for recursive listing. 
     *
     * @param path the pathname of the directory
     * @param recurse recurse listing into subdirectory's
     * @return the Iterator over the paths contained in the given directory
     * relative to the root
     * @see Iterator
     */
    public Iterator<String> getDirListing(String path, boolean recurse) throws StorageException;

    /**
     * Returns a Map object with a mapping from relative path (String) to full versioning
     * info (StorageDirEntry) of that path. This function can return directory
     * listings from all previous revisions.
     * 
     * @param path the pathname of the directory
     * @param revision the revision to list from 
     * @param recurse recurse listing into subdirectory's
     * @return The Map with the paths contained in the given directory
     * relative to the root as keys, and the corresponding StorageDirEntries as values.
     * @see Map
     * @see StorageDirEntry
     */
    public Map<String, StorageDirEntry> getDirListing(String path, long revision, boolean recurse) throws StorageException;

    /**
     * Return a map containing the status of the given path, in case of a directory all contents
     * will also be displayed nonrecursively. Same as calling {@link getStatus(String, boolean)}.
     *
     * @param path the pathname of the directory or file
     * @return Map (String, StorageStatusMessage) the map containing entries with the keys
     * set to the absolute paths and values set to the corresponding StorageStatusMessage
     * @see Map
     * @see StorageStatusMessage
     */
     public Map<String, StorageStatusMessage> getStatus(String path) throws StorageException;

    /**
     * Return a map containing the status of the given path, in case of a directory all contents
     * will also be displayed either recursively or nonrecursively.
     *
     * @param path the pathname of the directory or file
     * @param recursive true if directory contents should be traversed recursively
     * @return Map (String, StorageStatusMessage) the map containing entries with the keys
     * set to the absolute paths and values set to the corresponding StorageStatusMessage
     * @see Map
     * @see StorageStatusMessage
     */
     public Map<String, StorageStatusMessage> getStatus(String path, boolean recursive) throws StorageException;
     
    /**
     * Checks if a path exists in the working copy
     *
     * @param path the name of the file or directory to check
     * @return <tt>true</tt> if the file exists, <tt>false</tt> otherwise
     */
    public boolean fileExists(String path) throws StorageException;

    /**
     * Checks if a path identifies a directory
     *
     * @param path the path to check
     * @return <tt>true</tt> if the path is a directory, <tt>false</tt> if it
     * is not, or the path does not exist
     */
    public boolean isDirectory(String path) throws StorageException;

    /**
     * Add directory to repository
     *
     * @param pathName the directory to add
     */
    public void makeDirectory(String pathName) throws StorageException;
    
    /**
     * Ensures all components of a path (e.g. /dir/foo/) exist, if
     * this is not the case, directories are created to fix this.
     * 
     * @param path The full path of directories to ensure existance of
     * @throws StorageException
     */
    public void ensurePathExists(String path) throws StorageException;

    /**
     * Moves or renames a file. Technically there's no difference
     * between moving and renaming.
     *
     * @param oldPath the old absolute path
     * @param newPath the new absolute path
     * @param force specifies if the move/rename should be forced
     */
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException;

    /**
     * Copies a file
     *
     * @param originalPath absolute path of the original file or directory
     * @param copiedPath absolute path of the new copy
     */
    public void copyFile(String originalPath, String copiedPath) throws StorageException;

    /**
     * Deletes a file or directory, this also removes all the 
     * directories contents.
     *
     * @param absolute path of the file or directory to delete
     * @param force if <tt>true</tt> delete the file or directory even
     * if there are local changes
     */
    public void deleteFile(String path, boolean force) throws StorageException;

    /**
     * Reverts a file or directory to the last revision, basically removing all
     * local changes to the path.
     * This does not undelete the contents of directories if it
     * was deleted in the working copy, it does however create
     * the directory.
     *
     * @param path absolute path to revert
     */
    public void revertFile(String path) throws StorageException;

    /**
     * Reverts a file to a previous revision
     *
     * @param path the file to revert
     * @param revision the revision to revert to
     */
    public void revertFile(String path, long revision) throws StorageException;

    /**
     * Returns the differences between two revisions of a file
     *
     * @param path the file to diff
     * @param revision1 the first revision
     * @param revision2 the second revision
     * @return the differences between the two specified revisions
     */
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException;

    /**
     * Returns a SortedSet with StorageLogMessages
     *
     * @param path the name of the file to log
     * @return the SortedSet (StorageLogMessage)
     * @see SortedSet
     */
    public SortedSet<StorageLogMessage> getLog(String path) throws StorageException;

    /**
     * Sets a property of a file to a certain value. Use null as the value if you
     * want to delete a property.
     *
     * @param path the name of the file to set a property for
     * @param property the name of the property to set
     * @param value the new value of the property
     * @param recurse set to <tt>true</tt> to recurse into subdirectories
     */
    public void setProperty(String path, String property, String value, boolean recurse) throws StorageException;

    /**
     * Returns a HashMap object containing the properties of a file
     *
     * @param path the name of the file to retrieve properties from
     * @return the HashMap (name (String), value (String))
     * @see HashMap
     */
    public Map<String, String> getProperties(String path) throws StorageException;

    /**
     * Commits the current changes
     *
     * @param message the log message to include in the commit
     * @return <tt>true</tt> on successful commit, <tt>false</tt> on conflicts
     */
    public boolean commit(String message) throws StorageException;

    /**
     * Updates all content to the most recent revision
     *
     */
    public void update() throws StorageException;

    /**
     * Returns the current conflicts
     *
     * @return Iterator (String) containing the pathnames of conflicting items
     */
    public Iterator<String> getConflicts() throws StorageException;

    /**
     * Sets the status of a file to "resolved" (after a merge conflict)
     *
     * @param path the name of the file to resolve
     */
    public void setResolved(String path) throws StorageException;

    /**
     * Starts a transaction (a set of changes)
     */
    public void beginTransaction() throws StorageException;

    /**
     * Cancels (aborts) a transaction, reverts any changes
     */
    public void cancelTransaction() throws StorageException;

    /**
     * Finishes (commits) a transaction
     * @param message the log message to include in the commit
     */
    public void endTransaction(String message) throws StorageException;

    /**
     * Sets the svn user. This methods allows changing the user at
     * runtime (possibly after logging in a user). The name of the
     * user is then used at commit time.
     * @param username The username.
     */
    public void setUsername(String username) throws StorageException;

}
