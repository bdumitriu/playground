package gw.storage.javasvn.pathtree;

import gw.storage.javasvn.Path;

/*
 * This Exception does not derive from StorageException because
 * it is not supposed to ever leave the storage module. This is
 * because it contains vulnerable data (the PathNode lastNode).
 */
public class NodeNotFoundException extends Exception
{
	private static final long serialVersionUID = 1L;
	
	private String[] path;
	private PathNode lastNode;
	private int lastIndex;
	
	public NodeNotFoundException(String[] path, PathNode lastNode, int lastIndex)
	{
		this.path = path;
		this.lastNode = lastNode;
		this.lastIndex = lastIndex;
	}
	
	public PathNode getLastNode()
	{
		return lastNode;
	}
	
	public Path getFullPath()
	{
		return new Path(path);
	}
	
	public Path getRemainingPath()
	{
		int size = path.length - lastIndex;
		String[] remainingPath = new String[size];
		
		for ( int i = 0; i < size; i++ )
			remainingPath[i] = path[i+lastIndex];
		
		return new Path(remainingPath);
	}
}
