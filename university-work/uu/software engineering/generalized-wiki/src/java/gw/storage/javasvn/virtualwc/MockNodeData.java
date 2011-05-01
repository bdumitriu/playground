package gw.storage.javasvn.virtualwc;

import java.io.InputStream;
import java.util.Map;

import gw.storage.StorageException;
import gw.storage.javasvn.pathtree.NodeData;


public class MockNodeData extends NodeData
{

	@Override
	public Map<String, String> getProperties() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public InputStream getFile() throws StorageException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getProperty(String name) throws StorageException {
		// TODO Auto-generated method stub
		return null;
	}

}
