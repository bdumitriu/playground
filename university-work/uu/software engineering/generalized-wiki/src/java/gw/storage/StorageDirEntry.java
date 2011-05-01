package gw.storage;

import java.util.*;

/**
 * Wrapper-class for directory info. Each node in the directorylisting is
 * represented by a StorageDirEntry.
 */
public abstract class StorageDirEntry
{
	
    @Override
	public final String toString()
    {
    	return
    			"isFile: " + isFile() +
    			" isDirectory: " + isDirectory() +
    			" Last Author: " + getLastChangedAuthor() +
    			" Last Changed On: " + getLastChangedDate() +
    			" Last Changed in revision: " + getLastChangedRevision();
	}

	@Override
	public final boolean equals(Object obj)
	{
		if ( obj instanceof StorageDirEntry )
		{
			StorageDirEntry other = (StorageDirEntry) obj;
			
			return
					this.isDirectory() == other.isDirectory() &&
					this.isFile() == other.isFile() &&
					this.getLastChangedAuthor().equals(other.getLastChangedAuthor()) &&
					this.getLastChangedDate().getTime() == other.getLastChangedDate().getTime() &&
					this.getLastChangedRevision() == other.getLastChangedRevision();
		}
		
		return false;
			
	}

	/**
     * Get the date this node (file/directory) was last changed in the
     * repository
     * 
     * @return the last change date
     */
    public abstract Date getLastChangedDate();

    /**
     * Get last revision this node (file/directory) was changed in the
     * repository
     * 
     * @return the last change revision
     */
    public abstract long getLastChangedRevision();

    /**
     * Get the responsible author for the last change to this node
     * (file/directory)
     * 
     * @return the last author who changed this node
     */
    public abstract String getLastChangedAuthor();

    /**
     * Get the date this node (file/directory) was last changed in the
     * repository
     * 
     * @return true if node represents directory, otherwise false
     */
    public abstract boolean isDirectory();

    /**
     * Get the date this node (file/directory) was last changed in the
     * repository
     * 
     * @return true if node represents a file, otherwise false
     */
    public abstract boolean isFile();
}
