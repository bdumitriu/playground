package gw.conference.test;

import gw.storage.MockStorageAdapter;
import gw.storage.StorageException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.*;

/** 
 * A simulated Storage class that handles file/directory existance and stores properties. 
 * @author LennartKats
 */
public class PropertyMockStorage extends MockStorageAdapter {
	/** All dirs/files and their map of properties */
	public Map<String, Map<String,String>> _existing
		= new HashMap<String, Map<String,String>>();
    public Map<String,String> _properties = new HashMap<String,String>();
    
	@Override
	public void ensurePathExists(String path) throws StorageException {
        path = stripEnd(path);
        if(_existing.get(path) == null)
            _existing.put(path, new HashMap<String,String>());
	}
    
    @Override
    public boolean fileExists(String path) throws StorageException {
        return _existing.containsKey(stripEnd(path));
    }
    
    @Override
    public OutputStream storeFile(String path) throws StorageException {
        _existing.put(stripEnd(path), new HashMap<String,String>());
        return new ByteArrayOutputStream();
    }
    
    public InputStream getFile(String path) throws StorageException {
        // To test the xml file thingie
        String xmlFile = "<conference> " +
                         " <paper name=\"1\" status=\"pending\">"+
                         " </paper> " + 
                         "</conference>";

        byte[] tmp = xmlFile.getBytes();
        InputStream is = new ByteArrayInputStream(tmp);   
        return is;
    }

	
	@Override
	public void setProperty(String path, String name, String value, boolean recurse) throws StorageException {
		if(recurse) notImplemented();
		
        if(name.indexOf(" ") != -1) throw new RuntimeException("Properties cannot contain spaces.");

        getProperties(path).put(name, value);
	}

    @Override
    public Iterator<String> getDirListing(String path) throws StorageException {
        path = stripEnd(path) + "/";
        ArrayList<String> result = new ArrayList<String>();
        
        for(String key : _existing.keySet()) {
            if(key.startsWith(path))
                result.add(key);
        }
        
        return result.iterator();
    }  
	
	@Override
	public Map<String,String> getProperties(String path) throws StorageException {
        Map<String,String> result = _existing.get(stripEnd(path));
        if(result == null) throw new StorageException("Path does not exist: " + path);
        return result;
	}
    
    private String stripEnd(String path) {
        return path.endsWith("/") ? path.substring(0, path.length() - 1) : path;
    }
}