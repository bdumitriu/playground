package gw.storage;

import java.util.*;

/**
 * Abstract implementation of StorageLogMessage interface
 */
public abstract class AbstractStorageLogMessage implements StorageLogMessage, Comparable
{
    /**
     * Utility method for ordering StorageLogMessages in Java Collections
     */
    public int compareTo(Object o)
    {
        StorageLogMessage storageLogMessage = (StorageLogMessage) o;

        Long revision1 = new Long(this.getRevisionNumber());
        Long revision2 = new Long(storageLogMessage.getRevisionNumber());
        return revision1.compareTo( revision2 );
    }

	@Override
	public boolean equals(Object obj)
	{
		if ( obj instanceof StorageLogMessage )
		{
			StorageLogMessage other =
				(StorageLogMessage) obj;
			
			// compare the authors
			if ( this.getAuthor() == null ^ other.getAuthor() == null )
				return false;

			if ( this.getAuthor() != null )
				if ( !this.getAuthor().equals(other.getAuthor()) )
						return false;

			// compare the dates
			if ( this.getDate() == null ^ other.getDate() == null )
				return false;

			if ( this.getDate() != null && other.getDate() != null )
				if ( this.getDate().getTime() != other.getDate().getTime() )
						return false;
			
			// compare the messages
			if ( this.getMessage() == null ^ other.getMessage() == null )
				return false;

			if ( this.getMessage() != null )
				if ( !this.getMessage().equals(other.getMessage()) )
						return false;
			
			// compare the changed paths
			if ( this.getChangedPaths() == null ^ other.getChangedPaths() == null )
				return false;
			
			if ( this.getChangedPaths() != null )
				if ( !this.getChangedPaths().equals(other.getChangedPaths()) )
						return false;
			
			// compare the revision numbers
			if ( this.getRevisionNumber() != other.getRevisionNumber() )
				return false;
			
			return true;
		}

		return false;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("r");
		sb.append(getRevisionNumber());
		sb.append(" | ");
		sb.append(getAuthor());
		sb.append(" | ");
		sb.append(getDate());
		sb.append("\nChanged paths:\n");
		for (Map.Entry<String, Character> entry :
			(Set<Map.Entry<String, Character>>) getChangedPaths().entrySet())
		{
		    sb.append("   ");
		    sb.append(entry.getValue());
		    sb.append(" ");
		    sb.append(entry.getKey());
		    sb.append("\n");
		}
		sb.append("\n");
		sb.append(getMessage());
		sb.append("\n");
		
		return sb.toString();
	}
}
