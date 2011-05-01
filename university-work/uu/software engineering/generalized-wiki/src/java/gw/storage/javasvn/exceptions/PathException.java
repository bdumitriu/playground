package gw.storage.javasvn.exceptions;

import gw.storage.StorageException;


public class PathException extends StorageException
{
	protected String path = "";
	protected long revision = -1;
	
	protected PathException(String message)
	{
		super(message);
	}
	
	public PathException(String path, long revision)
	{
		super("Path " + path + " does not exist in revision " + revision + "!");
		
		this.path = path;
		this.revision = revision;
	}
	
	public String getPath() { return path; }
	
	public long getRevision() { return revision; }
}
