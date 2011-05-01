package gw.storage;

import java.util.Date;

/**
 * Interface for status messages
 *
 * One status message is created for a single path (directory or file)
 */
public abstract class StorageStatusMessage
{
	
	
	@Override
	public final boolean equals(Object obj)
	{
		if ( obj instanceof StorageStatusMessage )
		{
			StorageStatusMessage other = (StorageStatusMessage) obj;
			StorageStatusMessage reference = this;
			
			return areEqual(reference.getLastChangedDate(), other.getLastChangedDate())
					&& reference.isFile() == other.isFile()
					&& reference.isDirectory() == other.isDirectory()
					&& reference.isModified() == other.isModified()
					&& reference.isDeleted() == other.isDeleted()
					&& reference.isCopied() == other.isCopied()
					&& reference.isAdded() == other.isAdded()
					&& reference.getRevision() == other.getRevision()
					&& areEqual(reference.getLastCommitAuthor(), other.getLastCommitAuthor())
					&& reference.getLastChangedRevision() == other.getLastChangedRevision();
		}
		
		return false;
	}
	
	private boolean areEqual(String o, String p)
	{
		if ( o == null )
		{
			if ( p == null )
				return true;
			
			return false;
		}
		
		return o.equals(p);
	}
	
	private boolean areEqual(Date o, Date p)
	{
		if ( o == null )
		{
			if ( p == null )
				return true;
			
			return false;
		}
		
		if ( p == null )
			return false;
		
		return o.getTime() == p.getTime();
	}

	@Override
	public String toString()
	{
		return "Author: " + getLastCommitAuthor()
			+ " LastChangedRev: " + getLastChangedRevision()
			+ " LastChangedDate: " + getLastChangedDate()
			+ " Revision: " + getRevision()
			+ " isFile: " + isFile()
			+ " isDirectory: " + isDirectory()
			+ " isDeleted: " + isDeleted()
			+ " isCopied: " + isCopied()
			+ " isAdded: " + isAdded()
			+ " isModified: " + isModified();
	}
	
    /**
     * Returns the date of the last change to this path
     *
     * @return the date of the last change
     */
    public abstract Date getLastChangedDate();
    
    /**
     * Returns the revision number of the last change to this path
     *
     * @return the number of the last revision
     */
    public abstract long getLastChangedRevision();
    
    /**
     * Returns the author of the last commit to this path
     *
     * @return the author of the last commit
     */
    public abstract String getLastCommitAuthor();
    
    /**
     * Returns the revision of the path. This higher than or equal to
     * the revision number returned by getLastChangedDate(). 
     *
     * @return the revision of the path
     */
    public abstract long getRevision();
    
    /**
     * Tests if the specified path is new (was added)
     *
     * @return <tt>true</tt> if the specified path is new
     */
    public abstract boolean isAdded();
    
    /**
     * Tests if the specified path was copied from some other path
     *
     * @return <tt>true</tt> if the specified path was copied
     */
    public abstract boolean isCopied();

    /**
     * Tests if the specified path was deleted
     *
     * @return <tt>true</tt> if the specified path was deleted
     */
    public abstract boolean isDeleted();

    /**
     * Tests if the specified path was modified
     *
     * @return <tt>true</tt> if the specified path was modified
     */
    public abstract boolean isModified();

    /**
     * Tests if the specified path is a directory
     *
     * @return <tt>true</tt> if the specified path is a directory
     */
    public abstract boolean isDirectory();
    
    /**
     * Tests if the specified path is a file
     *
     * @return <tt>true</tt> if the specified path is a file
     */
    public abstract boolean isFile();

}