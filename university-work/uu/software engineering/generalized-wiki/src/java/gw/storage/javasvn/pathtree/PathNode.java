package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;
import gw.storage.javasvn.exceptions.NotADirException;
import gw.storage.javasvn.exceptions.PathExistsException;

import java.util.Collection;
import java.util.HashMap;
import java.util.Stack;


public class PathNode
{
	private String path;
	private PathNode parent;
	private HashMap<String, PathNode> children;
	private NodeData data;
	private boolean isDir;
	
	/**
	 * Used to construct a rootnode.
	 *  
	 * @see RootNode
	 * @see PathNode(String, isDir)
	 */
	public PathNode()
	{
		this.isDir = true;
		this.path = "";
		this.parent = null;
		this.data = null;
		this.children = new HashMap<String, PathNode>();
	}

	public PathNode(String path, boolean isDir)
	{
		assert path != "";
		
		this.path = path;
		this.isDir = isDir;
		this.parent = null;
		this.data = null;
		
		if ( isDir )
			children = new HashMap<String, PathNode>();
	}
	
	public String getPath() { return path; }
	
	public Path getFullPath()
	{
		Stack<String> pathStack = new Stack<String>();
		
		PathNode current = this;
		
		while ( current != null )
		{
			pathStack.push(current.getPath());
			current = current.getParent();
		}
		
		// pop the root which we don't need
		pathStack.pop();
		
		// dunno why but this somehow results in a castexception
		//return new Path((String[]) pathStack.toArray());
		// that's why I do it manually:
		
		String[] pathArray = new String[pathStack.size()];
		
		for ( int i = 0; i < pathArray.length; i++ )
			pathArray[i] = pathStack.pop();
			
		return new Path(pathArray);
	}
		
	public void addChild(PathNode child) throws NotADirException, StorageException
	{
		if ( isFile() )
			throw new NotADirException(getFullPath());
		
		if ( children.containsKey(child.getPath()) )
			throw new PathExistsException(getFullPath() + "/" + child.getPath());
		
		// Only the root can have an empty pathname!
		assert child.getPath() == "";
			
		children.put(child.getPath(), child);
		
		child.setParent(this);
	}
	
	protected void removeChild(String childPath) throws StorageException
	{
		PathNode child = getChild(childPath);
		
		if ( child != null )
		{
			child.parent = null;
			children.remove(childPath);
		}
	}
	
	public PathNode getChild(String childPath) throws StorageException
	{
		if ( isFile() )
			return null;

		return children.get(childPath);
	}
	
	public Collection<PathNode> getChildren() throws StorageException
	{
		if ( children == null )
			return null;
		
		return children.values();
	}
	
	public boolean isDir() { return isDir; }
	public boolean isFile() { return !isDir; }
	
	public NodeData getData() { return data; }
	
	public void setData(NodeData newData)
	{
		newData.setOwner(this);
		data = newData;
		
		// it's got to be a delete and an add because we cannot
		// change this dir to a file, if it is, change isDir
		if ( newData instanceof FileChanges && isDir() 
				|| newData instanceof DirectoryChanges && isFile() )
		{
				assert newData.isDelete();
				assert newData.isAdd();
				isDir = !isDir;
		}
	}
	
	public PathNode getParent() { return parent; }
	
	protected void setParent(PathNode parent) { this.parent = parent; }
	
	public boolean hasChanges()
	{
		return data instanceof ChangedNodeData;
	}
	
	public boolean isLogicallyDeleted()
	{
		if ( data == null )
			return false;
		
		return data.isDelete() && !data.isAdd();
	}
	
	public PathNode getNode(Path path) throws NodeNotFoundException, StorageException
	{
		PathNode current = this;
		String[] subPaths = path.toArray();
		
		// get as far as possible in the already existing dirs
		for ( int i = 0; i < subPaths.length; i++ )
		{
			PathNode child;
			
			child = current.getChild(subPaths[i]);
			
			if ( child == null || child.isLogicallyDeleted() )
				throw new NodeNotFoundException(subPaths, current, i);
			else
				current = child;
		}
				
		return current;
	}
	
	public RootNode getRoot()
	{
		PathNode current = this;
		
		while ( !(current instanceof RootNode) )
		{
			current = current.getParent();
		}
		
		return (RootNode) current;
	}
}
