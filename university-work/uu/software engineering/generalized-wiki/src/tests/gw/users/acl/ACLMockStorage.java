package gw.users.acl;

import com.mockobjects.ExpectationValue;

import gw.storage.MockStorageAdapter;
import gw.storage.StorageException;
import java.util.*;
import java.io.*;


/**
 * Mock storage object, used for testing purposes.
 */
public class ACLMockStorage extends MockStorageAdapter {
    private ExpectationValue _aclTest = new ExpectationValue("/aclTest");
    private Map _propsRoot  = new HashMap();
	private Map _propsFile1 = new HashMap();
    private Map _propsFile2 = new HashMap();
    private Map _propsFile3 = new HashMap();
    private Map _propsFile4 = new HashMap();
    private Map _propsFile5 = new HashMap();
    
	
	public void setExpectedConfigFile(final String aclTest){ 
	   _aclTest.setExpected(aclTest); 
    } 
	
    	
	public void setProperty( String path, String property, String value, boolean recurse) throws StorageException {
        if (path.equals("/aclTest"))
            _propsFile1.put( property, value );
        else if (path.equals("/aclTest/File2"))
            _propsFile2.put( property, value );
        else if (path.equals("/aclTest/File3"))
            _propsFile3.put( property, value );
        else if (path.equals("/Groups"))
            _propsFile4.put( property, value );
        else if (path.equals("/Groups/All"))
            _propsFile5.put( property, value);
        else
			throw new StorageException( "Setting property of non existent file" );
	}

	
    public Map getProperties( String path ) throws StorageException {
        if (path.equals("/aclTest"))
            return _propsFile1;
        else if (path.equals("/aclTest/File2"))
            return _propsFile2;
        else if (path.equals("/aclTest/File3"))
            return _propsFile3;
        else if (path.equals("/Groups"))
            return _propsFile4;
        else if (path.equals("/Groups/All"))
            return _propsFile5;
        else if (path.equals("/"))
            return _propsRoot;
        else
            throw new StorageException( "Getting properties of unknown file" );
	}
    
    
    public Iterator getDirListing(String path) throws StorageException {
        if (path.equals("/aclTest")) {
            ArrayList list = new ArrayList();
            list.add("/aclTest/File2");
            list.add("/aclTest/File3");
            return list.iterator();
        }
        else if (path.equals("/Groups")) {
            ArrayList list = new ArrayList();
            list.add("/Groups/All");
            list.add("/Groups/Singleton");
            list.add("/Groups/NoBody");
            return list.iterator();
        }
        else
            throw new StorageException( "Getting contents of unknow directory." );
    }
    
    
    public InputStream getFile(String path) throws StorageException {
        if (path.equals("/Groups/All")) {
            String members = "Jan\nPiet\nKlaas\nMiep";
            return new ByteArrayInputStream(members.getBytes());
        }
        else if (path.equals("/Groups/Singleton")) {
            String members = "Jan";
            return new ByteArrayInputStream(members.getBytes());
        }
        else if (path.equals("/Groups/NoBody")) {
            return new ByteArrayInputStream("".getBytes());
        }
        else if (path.equals("/Groups/DefaultGroup")) {
            return new ByteArrayInputStream("".getBytes());
        }
        else
            throw new StorageException( "No such file: " + path + ".");
    }
	
	
    public boolean commit(String message) throws StorageException {
		return true;
	}
    
    public boolean isDirectory(String path) throws StorageException {
        return true;
    }
    
    public boolean fileExists(String path) throws StorageException {
        return true;
    }
    
    public OutputStream storeFile(String path) throws StorageException {
        return new ByteArrayOutputStream();
    }
}
