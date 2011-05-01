package gw.storage.javasvn.exceptions;

import gw.storage.javasvn.Path;

public class PathExistsException extends PathException
{
	public PathExistsException(Path path)
	{
		this(path.toString());
	}
	
	public PathExistsException(String path)
	{
		super("Path " + path + " already exists!");
		this.path = path;
	}

}
