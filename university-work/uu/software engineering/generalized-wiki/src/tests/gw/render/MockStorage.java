package gw.render;

import gw.GwConstants;
import gw.storage.Storage;
import gw.storage.StorageException;
import gw.storage.StorageListener;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;

import com.mockobjects.*;

public class MockStorage extends MockObject implements Storage {

    private final ExpectationValue requestedFile = new ExpectationValue("requestedFile");
    private InputStream fileInputStream;
    private Map pathProperties;
    private Map pathInputStreams;

    public MockStorage() {
    	this("");
    }
    
    public MockStorage(String file) {
    	this.fileInputStream = new ByteArrayInputStream(file.getBytes());
    	pathProperties = new HashMap();
    	pathInputStreams = new HashMap();
    }
    
    public void setExpectedRequestedFile(String requestedFile){ 
        this.requestedFile.setExpected(requestedFile); 
    } 
	
	public OutputStream storeFile(String path) throws StorageException {
		notImplemented();
		return null;
	} 

	public InputStream getFile(String path) throws StorageException {
		InputStream input = (InputStream) pathInputStreams.get(path);
		
        if(path.equals("/FooBar")) {
            return new ByteArrayInputStream( (new String("FooBarContent")).getBytes());
        } else if(input != null) {
			return input;
		} else {			
			requestedFile.setActual(path);
			return fileInputStream;
		}
	}

	public InputStream getFile(String path, long revision) throws StorageException {
		notImplemented();
		return null;
	}

	public boolean fileExists(String path) throws StorageException {
		if("/stylesheets/view_application-gwml_xml.xsl".equals(path))
			return true;
		else if ("/foo/stylesheets/view_application-gwml_xml.xsl".equals(path))
			return true;
		else if (path.equals("/foo/file1.txt") ||
				 path.equals("/foo/x") ||
				 path.equals("/bar/dir/y") ||
				 path.equals("/file1.txt") ||
				 path.equals("/x"))
			return true;
		else if(path.equals("/FooBar")) 
			return true;
        else
            return pathInputStreams.get(path) != null || pathProperties.get(path) != null;
	}

	public boolean isDirectory(String path) throws StorageException {
		if (path.equals("/") ||
			path.equals("/foo") ||
			path.equals("/foo/") ||
			path.equals("/bar") ||
			path.equals("/bar/") ||
			path.equals("/bar/dir") ||
			path.equals("/bar/dir/"))
			return true;
		else
			return false;
	}

	public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
		notImplemented();
	}

	public void copyFile(String originalPath, String copiedPath) throws StorageException {
		notImplemented();
	}

	public void deleteFile(String path, boolean force) throws StorageException {
		notImplemented();
	}

	public void revertFile(String path) throws StorageException {
		notImplemented();
	}

	public void revertFile(String path, long revision) throws StorageException {
		notImplemented();
	}

	public String getFileDiff(String path, long revision1, long revision2) throws StorageException {
		notImplemented();
		return null;
	}

	public SortedSet getLog(String path) throws StorageException {
		notImplemented();
		return null;
	}

	public void setProperty(String path, String property, String value, boolean recurse) throws StorageException {
		notImplemented();
	}

	public Map getProperties(String path) throws StorageException {
		if(path.equals("/FooBar")){
			HashMap map = new HashMap();
			map.put("content-type", GwConstants.GWML_MIME_TYPE);
			return map;
		}
		return (Map) pathProperties.get(path);
	}

	public boolean commit(String message) throws StorageException {
		notImplemented();
		return false;
	}

	public Iterator getConflicts() throws StorageException {
		notImplemented();
		return null;
	}

	public void setResolved(String path) throws StorageException {
		notImplemented();
	}

	public void addListener(StorageListener listener) {
		notImplemented();
	}

	public void removeListener(StorageListener listener) {
		notImplemented();
	}
    /**
     * Starts a transaction
     */
    public void beginTransaction() throws StorageException {
      notImplemented();
    }

    /**
     * Cancels a transaction
     */
    public void cancelTransaction() throws StorageException {
      notImplemented();
    }

    /**
     * Finishes (commits) a transaction
     */
    public void endTransaction(String message) throws StorageException {
      notImplemented();
    }

    public void update() throws StorageException {
      notImplemented();
    }

    public void makeDirectory(String pathName) throws StorageException {
      notImplemented();
    }

    public void ensurePathExists(String path) throws StorageException {
      notImplemented();
    }

    public Map getStatus(String path) throws StorageException {
      notImplemented();
      return null;
    }

    public Map getStatus(String path, boolean recursive) throws StorageException {
      notImplemented();
      return null;
    }

    public Iterator getDirListing(String path) throws StorageException {
      notImplemented();
      return null;
    }

    public Iterator getDirListing(String path, boolean recurse) throws StorageException {
      notImplemented();
      return null;
    }

    public Map getDirListing(String path, long revision, boolean recurse) throws StorageException {
      notImplemented();
      return null;
    }

    public Iterator blame(String path, long revisionStart, long revisionEnd) throws StorageException {
      notImplemented();
      return null;
    }

	public void setProperty(String path, String name, String value) {
    	Map properties = new HashMap();
    	properties.put(name, value);
		pathProperties.put(path, properties);
	}
    
    public void setUsername(String username) throws StorageException {
    }

	/**
	 * @param path
	 * @param object
	 */
	public void addInputStream(String path, InputStream input) {
		pathInputStreams.put(path, input); 		
	}
}
