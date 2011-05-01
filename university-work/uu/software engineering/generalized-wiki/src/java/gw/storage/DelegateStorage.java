package gw.storage;


import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.io.OutputStream;
import java.io.InputStream;


/**
 * The delegate storage object delegates all calls to the
 * provided delegate. It can be used as base class for a
 * storage decorator.
 */
public abstract class DelegateStorage implements Storage {
    protected Storage _delegate;


    /**
     * Creates a storage object by delegating all calls to the
     * the delegate storage object.
     * @param delegate
     */
    public DelegateStorage(Storage delegate) {
        if (delegate == null)
            throw new IllegalArgumentException("delegate is null.");

        _delegate = delegate;
    }


    /**
     * Retrieve the content together with the author, the revision and the date of the last change of each line
     *
     * @param path the name of the file to write to
     * @param revisionStart the first revision to show
     * @param revisionEnd the last revision to show
     * @return Iterator containing StorageBlameLine objects
     */
    public Iterator<StorageBlameLine> blame(String path, long revisionStart, long revisionEnd) throws StorageException {
        return _delegate.blame(path, revisionStart, revisionEnd);
    }


    /**
     * Returns an OutputStream object to write to a file
     *
     * @param path the name of the file to write to
     * @return the OutputStream object
     * @see OutputStream
     */
    public OutputStream storeFile(String path) throws StorageException {
        return _delegate.storeFile(path);
    }


    /**
     * Returns an InputStream object to read from a file
     *
     * @param path the name of the file to read from
     * @return the InputStream object (empty when file does not exist).
     * @exception StorageException when path is a directory.
     * @see InputStream
     */
    public InputStream getFile(String path) throws StorageException {
        return _delegate.getFile(path);
    }

    /**
     * Returns an InputStream object to read from a file with a certain revision
     *
     * @param path the name of the file to read from
     * @param revision the specified revision
     * @return the InputStream object
     * @see InputStream
     */
    public InputStream getFile(String path, long revision) throws StorageException {
        return _delegate.getFile(path, revision);
    }


    /**
     * Returns an Iterator object to iterate over the contents of a directory
     * Is non-recursive by default. For recursive directory listing getDirListing(path, recurse)
     *
     * @param path the pathname of the directory
     * @return the Iterator (String) object
     * @see Iterator
     */
    public Iterator<String> getDirListing(String path) throws StorageException {
        return _delegate.getDirListing(path);
    }

    public Iterator<String> getDirListing(String path, boolean recurse) throws StorageException {
        return _delegate.getDirListing(path, recurse);
    }

    public Map<String, StorageDirEntry> getDirListing(String path, long revision, boolean recurse) throws StorageException {
        return _delegate.getDirListing(path, revision, recurse);
    }


    /**
     * Returns a Map containing a mapping from path to StorageStatusMessage
     *
     * @param path the pathname of the directory or file
     * @return Map (String, StorageStatusMessage)
     * @see Map
     */
    public Map<String, StorageStatusMessage> getStatus(String path) throws StorageException {
        return _delegate.getStatus(path);
    }

    /**
     * Returns a Map containing a mapping from path to StorageStatusMessage
     *
     * @param path the pathname of the directory or file
     * @param recursive true if recursive, false otherwise.
     * @return Map (String, StorageStatusMessage)
     * @see Map
     */
    public Map<String, StorageStatusMessage> getStatus(String path, boolean recursive) throws StorageException {
        return _delegate.getStatus(path, recursive);
    }


    /**
     * Checks if a file exists
     *
     * @param path the name of the file to check
     * @return <tt>true</tt> if the file exists, <tt>false</tt> otherwise
     */
    public boolean fileExists(String path) throws StorageException {
        return _delegate.fileExists(path);
    }


    /**
     * Checks if a path is a directory
     *
     * @param path the path to check
     * @return <tt>true</tt> if the path is a directory, <tt>false</tt> otherwise
     */
    public boolean isDirectory(String path) throws StorageException {
        return _delegate.isDirectory(path);
    }


    /**
     * Add directory to repository
     *
     * @param pathName the directory to add
     */
    public void makeDirectory(String pathName) throws StorageException {
        _delegate.makeDirectory(pathName);
    }

    /**
     * @param pathName
     * @throws StorageException
     */
    public void ensurePathExists(String path) throws StorageException {
        _delegate.ensurePathExists(path);
    }


    /**
     * Moves or renames a file
     *
     * @param oldPath the old name
     * @param newPath the new name
     * @param force specifies if the move/rename should be forced
     */
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
        _delegate.moveFile(oldPath, newPath, force);
    }


    /**
     * Copies a file
     *
     * @param originalPath the original name
     * @param copiedPath the name for the copy
     */
    public void copyFile(String originalPath, String copiedPath) throws StorageException {
        _delegate.copyFile(originalPath, copiedPath);
    }


    /**
     * Deletes a file
     *
     * @param path the file to delete
     * @param force specifies if the move/rename should be forced     *
     */
    public void deleteFile(String path, boolean force) throws StorageException {
       _delegate.deleteFile(path, force);
    }


    /**
     * Reverts a file to a previous revision
     *
     * @param path the file to revert
     */
    public void revertFile(String path) throws StorageException {
        _delegate.revertFile(path);
    }

    /**
     * Reverts a file to a previous revision
     *
     * @param path the file to revert
     * @param revision the revision to revert to
     */
    public void revertFile(String path, long revision) throws StorageException {
        _delegate.revertFile(path, revision);
    }


    /**
     * Returns the differences between two revisions of a file
     *
     * @param path the file to diff
     * @param revision1 the first revision
     * @param revision2 the second revision
     * @return the differences between the two specified revisions
     */
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException {
        return _delegate.getFileDiff(path, revision1, revision2);
    }


    /**
     * Returns a SortedSet with StorageLogMessages
     *
     * @param path the name of the file to log
     * @return the SortedSet (StorageLogMessage)
     * @see SortedSet
     */
    public SortedSet<StorageLogMessage> getLog(String path) throws StorageException {
        return _delegate.getLog(path);
    }


    /**
     * Sets a property of a file to a certain value
     *
     * @param path the name of the file to set a property for
     * @param property the name of the property to set
     * @param value the new value of the property
     * @param recurse set to <tt>true</tt> to recurse into subdirectories
     */
    public void setProperty(String path, String property, String value, boolean recurse) throws StorageException {
        _delegate.setProperty(path, property, value, recurse);
    }


    /**
     * Returns a HashMap object containing the properties of a file
     *
     * @param path the name of the file to retrieve properties from
     * @return the HashMap (name (String), value (String))
     * @see java.util.Map
     */
    public Map<String, String> getProperties(String path) throws StorageException {
        return _delegate.getProperties(path);
    }


    /**
     * Commits the current changes
     *
     * @param message the log message to include in the commit
     * @return <tt>true</tt> on successful commit, <tt>false</tt> on conflicts
     */
    public boolean commit(String message) throws StorageException {
        return _delegate.commit(message);
    }


    /**
     * Updates all content to the most recent revision
     *
     */
    public void update() throws StorageException {
        _delegate.update();
    }


    /**
     * Returns the current conflicts
     *
     * @return Iterator (String) containing the pathnames of conflicting items
     */
    public Iterator<String> getConflicts() throws StorageException {
        return _delegate.getConflicts();
    }


    /**
     * Sets the status of a file to "resolved" (after a merge conflict)
     *
     * @param path the name of the file to resolve
     */
    public void setResolved(String path) throws StorageException {
        _delegate.setResolved(path);
    }


    /**
     * Starts a transaction (a set of changes)
     */
    public void beginTransaction() throws StorageException {
        _delegate.beginTransaction();
    }


    /**
     * Cancels (aborts) a transaction, reverts any changes
     */
    public void cancelTransaction() throws StorageException {
        _delegate.cancelTransaction();
    }


    /**
     * Finishes (commits) a transaction
     * @param message the log message to include in the commit
     */
    public void endTransaction(String message) throws StorageException {
        _delegate.endTransaction(message);
    }
    
    /**
     * Sets the subversion username.
     * @see gw.storage.Storage#setUsername(java.lang.String)
     */
    public void setUsername(String username) throws StorageException {
        _delegate.setUsername(username);    
    }
}
