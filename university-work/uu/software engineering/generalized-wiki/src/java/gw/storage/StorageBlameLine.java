package gw.storage;

import java.util.*;


/**
 * A class containing blame information for a line.
 */
public class StorageBlameLine
{
    private Date   _changed;
    private long   _revision;
    private String _author;
    private String _line;

    /**
     * Constructor
     *
     * @param changed the date of the change
     * @param revision the revision of the change
     * @param author the author responsible for the change
     * @param line the individual line
     */
    public StorageBlameLine(Date changed,
                            long revision,
                            String author,
                            String line)
    {
        _changed = changed;
        _revision = revision;
        _author = author;
        _line = line;
    }

    /**
     * get blame changed date
     *
     * @return changed date
     */
    public Date getChanged()
    {
        return _changed;
    }

    /**
     * get blame revision
     *
     * @return revision
     */
    public long getRevision()
    {
        return _revision;
    }

    /**
     * get blame author
     *
     * @return author
     */
    public String getAuthor()
    {
        return _author;
    }

    /**
     * get blame line
     *
     * @return line
     */
    public String getLine()
    {
        return _line;
    }

	@Override
	public boolean equals(Object obj)
	{
		// this function has mainly been implemented to aid unit tests

		if ( obj instanceof StorageBlameLine )
		{
			StorageBlameLine other = (StorageBlameLine) obj;
			
			return other.getAuthor().equals(this.getAuthor())
					&& other.getChanged().getTime() == this.getChanged().getTime()
					&& other.getRevision() == this.getRevision()
					&& other.getLine().equals(this.getLine());
			
		}
		else
		{
			return false;
		}
	}
	
	@Override
	public String toString()
	{
		return "Date: " + getChanged() +
			" Author: " + getAuthor() +
			" Revision: " + getRevision() +
			" Line: " + getLine();
	}
}
