package gw.storage.javasvn.pathtree;


import gw.storage.StorageException;

import java.util.HashMap;
import java.util.Map;



public abstract class ChangedNodeData extends NodeData
{
	private NodeData original;
	
	protected boolean isDelete = false;
	protected boolean isAdd = false;
	private Map<String, String> properties = null;
	
	public ChangedNodeData(NodeData predecessor)
	{
		if ( predecessor == null )
		{
			original = null;
			isDelete = false;
		}
		else
		{
			original = predecessor.getOriginalData();
			isDelete = predecessor.isDelete();
		}
	}	
	
	public boolean isModFile() { return false; }
	
	public boolean isModProperty() { return false; }
	
	public boolean isCopy() { return false; }
	
	public boolean isMove() { return false; }
	
	public boolean isAdd() { return isAdd; }
	
	public boolean isDelete() { return isDelete; }
	
	public NodeData getOriginalData() { return original; }
	
	public Map<String, String> getProperties() throws StorageException
	{
		HashMap<String, String> allProperties;
		
		if ( properties == null )
			allProperties = new HashMap<String, String>(properties);
		else
			allProperties = new HashMap<String, String>();
		
		if ( original != null )
			allProperties.putAll(original.getProperties());
		
		return allProperties;
	}
	
	public String getProperty(String name) throws StorageException
	{
		String value = properties.get(name);
		
		if ( value == null && original != null )
			value = original.getProperty(name);
			
		return value;
	}
	
	protected void setProperties(Map<String, String> properties)
	{
		// this can only be set exactly once
		assert properties == null;
		
		this.properties = properties;
	}

}
