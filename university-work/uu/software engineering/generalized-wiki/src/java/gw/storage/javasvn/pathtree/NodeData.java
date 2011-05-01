package gw.storage.javasvn.pathtree;

import gw.storage.StorageException;

import java.io.InputStream;
import java.util.Map;

public abstract class NodeData
{
	private PathNode owner = null;
	
	public boolean isModFile() { return false; }
	
	public boolean isModProperty() { return false; }
	
	public boolean isCopy() { return false; }
	
	public boolean isMove() { return false; }
	
	public boolean isAdd() { return false; }
	
	public boolean isDelete() { return false; }
	
	public NodeData getOriginalData() { return this; }
	
	public abstract InputStream getFile() throws StorageException;
	
	public abstract Map<String, String> getProperties() throws StorageException;
	
	public abstract String getProperty(String name) throws StorageException;
	
	public PathNode getOwner() { return owner; }
	
	public void setOwner(PathNode owner)
	{
		// this can only be set exactly once
		assert owner == null;
		
		this.owner = owner;
	}
}
