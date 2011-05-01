package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.exceptions.NotAFileException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.tmatesoft.svn.core.ISVNDirEntryHandler;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.SVNRepository;

public class ProxyNodeData extends NodeData
{
	Map<String, String> properties = null;
	byte[] contents = null;
	
	public ProxyNodeData() {}
	
	@Override
	public InputStream getFile() throws StorageException
	{
		if ( getOwner().isDir() )
			throw new NotAFileException(getOwner().getPath());
			
		if ( contents != null )
			return new ByteArrayInputStream(contents);

		// we have no local contents so retrieve them
		RootNode root = getOwner().getParent().getRoot();
		SVNRepository repository = root.getRepository();
		long revision = root.getRevision();
		String path = getOwner().getFullPath().toString(true);
		
		try
		{
			ByteArrayOutputStream os = new ByteArrayOutputStream();
			repository.getFile(path, revision, null, os);
			os.close();
			contents = os.toByteArray();
		}
		catch (IOException e)
		{
			throw new StorageException(e);
		}
		catch (SVNException e)
		{
			throw new StorageException(e);
		}
		
		return new ByteArrayInputStream(contents);
	}

	@Override
	public Map<String, String> getProperties() throws StorageException
	{
		if ( properties != null )
			return properties;
		
		// we have no cached properties so get them
		RootNode root = getOwner().getRoot();
		SVNRepository repository = root.getRepository();
		long revision = root.getRevision();
		String path = getOwner().getFullPath().toString(true);
		properties = new HashMap<String,String>();
		
		try
		{
			if ( getOwner().isFile() )
				repository.getFile(path, revision, properties, null);
			else
				repository.getDir(path, revision, properties, (ISVNDirEntryHandler) null);
		}
		catch (SVNException e)
		{
			properties = null;
			throw new StorageException(e);
		}
		
		return properties;
	}
	
	public String getProperty(String name) throws StorageException
	{
		return getProperties().get(name);
	}
	
	/**
	 * This is only to be called by a checkoutEditor, not when
	 * changing properties. Use the ChangedDataNodes for that.
	 * 
	 * If you use this, make sure you really add all properties
	 * because the properties will not be proxied anymore when
	 * any property is set.
	 * 
	 * @param properties
	 */
	public void addProperty(String name, String value)
	{
		if ( properties == null )
			properties = new HashMap<String,String>();
		
		properties.put(name, value);
	}
}
