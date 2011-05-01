package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;

import java.io.InputStream;
import java.util.HashMap;


public class DirectoryChanges extends ChangedNodeData
{
	
	protected DirectoryChanges(NodeData oldData)
	{
		super(oldData);
	}
	
	public static void addDir(PathNode node)
	{
		DirectoryChanges dc = new DirectoryChanges(node.getData());
		
		dc.isAdd = true;
		dc.setProperties(new HashMap<String, String>());
		
		node.setData(dc);
	}

	@Override
	public InputStream getFile() throws StorageException
	{
		// TODO: throw exception?
		return null;
	}

}
