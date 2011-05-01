package gw.storage.javasvn;

public class Path
{
	private String[] subPaths;
	
	public Path(String path)
	{
		String cleanPath = removeSlashes(path);
		if ( cleanPath.length() == 0 )
			subPaths = new String[0];
		else
			subPaths = cleanPath.split("/");
	}
	
	public Path(String[] path)
	{
		subPaths = path.clone();
	}
	
	public Path(Path other)
	{
		subPaths = other.toArray();
	}
	
	public String[] toArray()
	{
		return subPaths.clone();
	}
	
	public String pop()
	{
		if ( subPaths.length == 0 )
		{
			return null;
		}
		
		String[] newPath = new String[subPaths.length-1];
		String last = subPaths[subPaths.length-1];
		
		for ( int i = 0; i < newPath.length; i++ )
			newPath[i] = subPaths[i];
		
		subPaths = newPath;
		
		return last;
	}
	
	@Override
	public String toString()
	{
		return toString(false);
	}
	
	public String toString(boolean noRootSlash)
	{
		if ( subPaths.length == 0 )
			return noRootSlash ? "" : "/";
		
		StringBuilder sb = new StringBuilder();
		
		for ( String path : subPaths )
		{
			sb.append("/");
			sb.append(path);
		}
		
		if ( noRootSlash )
			return sb.substring(1);
		
		return sb.toString();
	}

	/**
     * Removes any leading or trailing slash from <code>str</code> and returns the
     * result.
     */
	static public String removeSlashes(String str)
    {
        String result = str;

        // remove any leading slashes
        while (result.startsWith("/"))
        {
            result = result.substring(1);
        }

        // remove any trailing slashes
        while (result.endsWith("/"))
        {
            result = result.substring(0, result.length() - 1);
        }

        int pos;
        while ((pos = result.indexOf("//")) != -1)
        {
        	result = result.substring(0, pos) + result.substring(pos+1, result.length());
        }
        
        return result;
    }
	
	static public String removeParents(String path)
	{
		path = removeSlashes(path);
		
		int lastSlash = path.lastIndexOf("/");
		
		return path.substring(lastSlash+1, path.length());
	}
}
