package gw.storage.javasvn.exceptions;

import gw.storage.javasvn.Path;


public class NotADirException extends PathException
{
	public NotADirException(String path, long revision)
	{
		super("Path " + path + " in revision " + revision + " is not a file!");
		
		this.path = path;
		this.revision = revision;
	}
	
	public NotADirException(String path)
	{
		super("Path " + path + " is not a file!");
		
		this.path = path;
	}
	
	public NotADirException(Path path)
	{
		this(path.toString());
	}

}
