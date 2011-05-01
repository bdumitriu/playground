package gw.storage;

import java.io.*;

import com.mockobjects.ExpectationValue;

/**
 * Object to Mock the Storageobject
 * @author John van Schie
 */
public class MockStorage extends MockStorageAdapter {

	private ExpectationValue _configFile = new ExpectationValue("configFile");
	private InputStream		 _configStream;
	
	public MockStorage( final String configFileContent ) {
		
		_configStream = new ByteArrayInputStream( configFileContent.getBytes() );	
	}
	
	public void setExpectedConfigFile(final String configFile){ 
		
		_configFile.setExpected(configFile); 
	} 
	
	public InputStream getFile(final String path) throws StorageException {
		_configFile.setActual(path);
		return _configStream;
	}
	
	public OutputStream storeFile(String path) throws StorageException {
		
		_configStream	= new ByteArrayInputStream( "Foo2:Bar2\n".getBytes() );
		fireFileModified( "configFile" );
		return null;
	}
	
	public void deleteFile(String path, boolean force) throws StorageException {
	
		this.fireFileDeleted( "configFile" );
	} 
	
	public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
	
		fireFileMoved( oldPath, newPath );	
    }
    
    public void setUsername(String username) throws StorageException {
    }
}
