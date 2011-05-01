package gw.storage.javasvn.pathtree;


import gw.storage.StorageException;
import gw.storage.javasvn.exceptions.NotAFileException;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;


public class FileChanges extends ChangedNodeData
{
	private boolean isModFile = false;
	private File file = null;
	
	protected FileChanges(NodeData oldData)
	{
		super(oldData);
	}
	
	public static void addFile(PathNode node, File newFile)
	{
		FileChanges fc = new FileChanges(node.getData());
		fc.isAdd = true;
		
		fc.setFile(newFile);
		fc.setProperties(new HashMap<String, String>());
		
		node.setData(fc);
	}
	
	public static void modifyFile(PathNode node, File newFile) throws NotAFileException
	{
		// we can only modify a file if the data was retrieved earlier
		assert node.getData() != null;
		
		if ( node.isDir() )
			throw new NotAFileException(node.getFullPath());
		
		FileChanges fc = new FileChanges(node.getData());
		
		if ( node.getData().isAdd() )
			fc.isAdd = true;
		else
			fc.isModFile = true;
		
		fc.setFile(newFile);
		fc.setProperties(new HashMap<String, String>());
		
		node.setData(fc);
	}

	@Override
	public boolean isModFile()
	{
		return isModFile;
	}
	
	private void setFile(File newFile)
	{
		this.file = newFile;
	}

	@Override
	public InputStream getFile() throws StorageException
	{
		if ( file == null )
			return getOriginalData().getFile();
			
		try
		{
			return new BufferedInputStream(new FileInputStream(file));
		}
		catch (FileNotFoundException e)
		{
			throw new StorageException(e);
		}
	}

}
