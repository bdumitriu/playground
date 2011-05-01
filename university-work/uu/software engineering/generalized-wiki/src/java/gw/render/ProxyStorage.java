package gw.render;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;

import java.util.HashMap;
import gw.ServletUtilities;

import gw.storage.Storage;
import gw.storage.StorageBlameLine;
import gw.storage.StorageDirEntry;
import gw.storage.StorageException;
import gw.storage.StorageLogMessage;
import gw.storage.StorageStatusMessage;

/**
 * Allows to register files which do not exits on disk. This is a delegate
 * object for a StorageImplementation.
 * 
 * FIXME move this to gw.storage (I was too scary to do so - this course is not
 * over yet)
 * 
 * FIXME some methods should not be redirected to the enclosed storage (i.e.
 * equals) (auto generated bug :D)
 * 
 * @see gw.storage.Storage
 */
public class ProxyStorage implements Storage {

    private static class FileEntry {
        InputStream input;
        Map<String, String> properties;

        FileEntry(InputStream input, Map<String, String> properties) {
            this.input = input;
            this.properties = properties;
        }
    }

    Storage storage;
    Map<String, FileEntry> virtualFiles;

    public ProxyStorage(Storage storage) {
        this.storage = storage;
        virtualFiles = new HashMap<String, FileEntry>();
    }

    /**
     * @throws StorageException
     */
    public void beginTransaction() throws StorageException {
        storage.beginTransaction();
    }

    /**
     * @param path
     * @param revisionStart
     * @param revisionEnd
     * @return
     * @throws StorageException
     */
    public Iterator<StorageBlameLine> blame(String path, long revisionStart, long revisionEnd)
            throws StorageException {
        return storage.blame(path, revisionStart, revisionEnd);
    }

    /**
     * @throws StorageException
     */
    public void cancelTransaction() throws StorageException {
        storage.cancelTransaction();
    }

    /**
     * @param message
     * @return
     * @throws StorageException
     */
    public boolean commit(String message) throws StorageException {
        return storage.commit(message);
    }

    /**
     * @param path
     * @param force
     * @throws StorageException
     */
    public void deleteFile(String path, boolean force) throws StorageException {
        storage.deleteFile(path, force);
    }

    /**
     * @param message
     * @throws StorageException
     */
    public void endTransaction(String message) throws StorageException {
        storage.endTransaction(message);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        return storage.equals(obj);
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public boolean fileExists(String path) throws StorageException {
        path = ServletUtilities.normalizePathInfo(storage, path);
        FileEntry file = virtualFiles.get(path);
        if (file != null)
            return true;
        return storage.fileExists(path);
    }

    /**
     * @return
     * @throws StorageException
     */
    public Iterator<String> getConflicts() throws StorageException {
        return storage.getConflicts();
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public Iterator<String> getDirListing(String path) throws StorageException {
        return storage.getDirListing(path);
    }

    /**
     * @param path
     * @param recurse
     * @return
     * @throws StorageException
     */
    public Iterator<String> getDirListing(String path, boolean recurse) throws StorageException {
        return storage.getDirListing(path, recurse);
    }

    /**
     * @param path
     * @param revision
     * @param recurse
     * @return
     * @throws StorageException
     */
    public Map<String, StorageDirEntry> getDirListing(String path, long revision, boolean recurse) throws StorageException {
        return storage.getDirListing(path, revision, recurse);
    }

    /**
     * Request an InputStream for some path. If this path is registered as a
     * virtual file, it will return that input stream. Not that this always the
     * same input stream for a path. So don't use it in different threads at the
     * same time. It will try to reset the input stream before returning it.
     * 
     * @param path
     * @return InputStream
     * @throws StorageException
     */
    public InputStream getFile(String path) throws StorageException {
        path = ServletUtilities.normalizePathInfo(storage, path);
        FileEntry file = (FileEntry) virtualFiles.get(path);
        if (file != null) {
            // reset to byte 0 for re-reading
            try {
                file.input.reset();
            } catch (IOException e) {
            }
            return file.input;
        }
        return storage.getFile(path);
    }

    /**
     * @param path
     * @param revision
     * @return
     * @throws StorageException
     */
    public InputStream getFile(String path, long revision) throws StorageException {
        // TODO if head revision then return the file?
        return storage.getFile(path, revision);
    }

    /**
     * @param path
     * @param revision1
     * @param revision2
     * @return
     * @throws StorageException
     */
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException {
        return storage.getFileDiff(path, revision1, revision2);
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public SortedSet<StorageLogMessage> getLog(String path) throws StorageException {
        return storage.getLog(path);
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public Map<String, String> getProperties(String path) throws StorageException {
        path = ServletUtilities.normalizePathInfo(storage, path);
        FileEntry file = (FileEntry) virtualFiles.get(path);
        if (file != null)
            return file.properties;
        return storage.getProperties(path);
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public Map<String, StorageStatusMessage> getStatus(String path) throws StorageException {
        return storage.getStatus(path);
    }

    /**
     * @param path
     * @param recursive
     * @return
     * @throws StorageException
     */
    public Map<String, StorageStatusMessage> getStatus(String path, boolean recursive) throws StorageException {
        return storage.getStatus(path, recursive);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return storage.hashCode();
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public boolean isDirectory(String path) throws StorageException {
        return storage.isDirectory(path);
    }

    /**
     * @param pathName
     * @throws StorageException
     */
    public void makeDirectory(String pathName) throws StorageException {
        storage.makeDirectory(pathName);
    }

    /**
     * @param pathName
     * @throws StorageException
     */
    public void ensurePathExists(String path) throws StorageException {
        storage.ensurePathExists(path);
    }

    /**
     * @param oldPath
     * @param newPath
     * @param force
     * @throws StorageException
     */
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
        storage.moveFile(oldPath, newPath, force);
    }

    /**
     * @param path
     * @throws StorageException
     */
    public void revertFile(String path) throws StorageException {
        storage.revertFile(path);
    }

    /**
     * @param path
     * @param revision
     * @throws StorageException
     */
    public void revertFile(String path, long revision) throws StorageException {
        storage.revertFile(path, revision);
    }

    /**
     * @param path
     * @param property
     * @param value
     * @param recurse
     * @throws StorageException
     */
    public void setProperty(String path, String property, String value, boolean recurse)
            throws StorageException {

        path = ServletUtilities.normalizePathInfo(storage, path);
        FileEntry file = virtualFiles.get(path);
        if (file != null) {
            file.properties.put(property, value);
        } else {
            storage.setProperty(path, property, value, recurse);
        }
    }

    /**
     * @param path
     * @throws StorageException
     */
    public void setResolved(String path) throws StorageException {
        storage.setResolved(path);
    }

    /**
     * @param username
     * @throws StorageException
     */
    public void setUsername(String username) throws StorageException {
        storage.setUsername(username);
    }

    /**
     * @param originalPath
     * @param copiedPath
     * @throws StorageException
     */
    public void copyFile(String originalPath, String copiedPath) throws StorageException {
        storage.copyFile(originalPath, copiedPath);
    }

    /**
     * @param path
     * @return
     * @throws StorageException
     */
    public OutputStream storeFile(String path) throws StorageException {
        return storage.storeFile(path);
    }

    /**
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return storage.toString();
    }

    /**
     * @throws StorageException
     */
    public void update() throws StorageException {
        storage.update();
    }

    public void addVirtualFile(String path, InputStream data, Map<String, String> properties)
            throws StorageException {
        path = ServletUtilities.normalizePathInfo(storage, path);
        virtualFiles.put(path, new FileEntry(data, properties));
    }
}
