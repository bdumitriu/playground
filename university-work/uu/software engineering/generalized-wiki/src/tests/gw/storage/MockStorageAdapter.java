package gw.storage;

import gw.render.parsers.XMLParser;
import gw.storage.AbstractStorage;
import gw.storage.StorageException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;

import com.mockobjects.Verifiable;
import com.mockobjects.util.Verifier;
import com.mockobjects.util.AssertMo;

/**
 * Adapter for MockStorage objects, all methods from the Storage interface
 * implemented with:<code>notImplemented();</code>.
 * Makes it easy to define a clean MockStorage object with only the properties you want.
 * @author John van Schie
 */
public class MockStorageAdapter extends AbstractStorage implements Verifiable {

    private HashMap<String, PipedInputStream> _files = new HashMap<String, PipedInputStream>();
    
    
	/* (non-Javadoc)
	 * @see gw.storage.Storage#blame(java.lang.String, long, long)
	 */
	public Iterator blame(String path, long revisionStart, long revisionEnd)
		throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#storeFile(java.lang.String)
	 */
	public OutputStream storeFile(String path) throws StorageException {
        notImplemented();
        return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getFile(java.lang.String)
	 */
	public InputStream getFile(String path) throws StorageException {
        notImplemented();
        return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getFile(java.lang.String, long)
	 */
	public InputStream getFile(String path, long revision) throws StorageException {
        return _files.get(path);
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getDirListing(java.lang.String)
	 */
	public Iterator getDirListing(String path) throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getDirListing(java.lang.String, boolean)
	 */
	public Iterator getDirListing(String path, boolean recurse)
		throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getDirListing(java.lang.String, long, boolean)
	 */
	public Map getDirListing(String path, long revision, boolean recurse)
		throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getStatus(java.lang.String)
	 */
	public Map getStatus(String path) throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getStatus(java.lang.String, boolean)
	 */
	public Map getStatus(String path, boolean recursive)
		throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#fileExists(java.lang.String)
	 */
	public boolean fileExists(String path) throws StorageException {
		notImplemented();
		return false;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#isDirectory(java.lang.String)
	 */
	public boolean isDirectory(String path) throws StorageException {
		notImplemented();
		return false;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#makeDirectory(java.lang.String)
	 */
	public void makeDirectory(String pathName) throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#moveFile(java.lang.String, java.lang.String, boolean)
	 */
	public void moveFile(String oldPath, String newPath, boolean force)
		throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#copyFile(java.lang.String, java.lang.String)
	 */
	public void copyFile(String originalPath, String copiedPath)
		throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#deleteFile(java.lang.String, boolean)
	 */
	public void deleteFile(String path, boolean force)
		throws StorageException {
		notImplemented();
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#revertFile(java.lang.String)
	 */
	public void revertFile(String path) throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#revertFile(java.lang.String, long)
	 */
	public void revertFile(String path, long revision)
		throws StorageException {
		notImplemented();
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getFileDiff(java.lang.String, long, long)
	 */
	public String getFileDiff(String path, long revision1, long revision2)
		throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getLog(java.lang.String)
	 */
	public SortedSet getLog(String path) throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#setProperty(java.lang.String, java.lang.String, java.lang.String, boolean)
	 */
	public void setProperty(
		String path,
		String property,
		String value,
		boolean recurse)
		throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getProperties(java.lang.String)
	 */
	public Map getProperties(String path) throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#commit(java.lang.String)
	 */
	public boolean commit(String message) throws StorageException {
		notImplemented();
		return false;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#update()
	 */
	public void update() throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#getConflicts()
	 */
	public Iterator getConflicts() throws StorageException {
		notImplemented();
		return null;
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#setResolved(java.lang.String)
	 */
	public void setResolved(String path) throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#beginTransaction()
	 */
	public void beginTransaction() throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#cancelTransaction()
	 */
	public void cancelTransaction() throws StorageException {
		notImplemented();

	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#endTransaction(java.lang.String)
	 */
	public void endTransaction(String message) throws StorageException {
		notImplemented();

	}
	
	// Verifiable implemention 
	public void verify(){ 
		
		Verifier.verifyObject(this); 
	} 
	
	public void notImplemented() {
		AssertMo.notImplemented(this.getClass().getName());
	}

	/* (non-Javadoc)
	 * @see gw.storage.Storage#setUsername(java.lang.String)
	 */
	public void setUsername(String username) throws StorageException {
        notImplemented();				
	}
}
