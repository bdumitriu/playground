package gw.storage.javasvn.exceptions;

import gw.storage.StorageException;
import gw.storage.javasvn.Path;

public class NotAFileException extends PathException
{
	public NotAFileException(String path, long revision)
	{
		super("Path " + path + " in revision " + revision + " is not a file!");
		
		this.path = path;
		this.revision = revision;
	}
	
	public NotAFileException(String path)
	{
		super("Path " + path + " is not a file!");
		
		this.path = path;
	}
	
	public NotAFileException(Path path)
	{
		super("Path " + path + " is not a file!");
		
		this.path = path.toString();
	}
}
