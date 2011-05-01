package gw.storage;

import java.util.Date;
import java.util.Map;

/**
 * Interface for log messages, composed of author, date, message, revision number and a list of changed files (paths)
 */
public interface StorageLogMessage
{

    /**
     * Returns the author of the log message. This is normally the user who made the commit.
     *
     * @return the author
     */
    public String getAuthor();
    
    /**
     * Returns a map with (path,status) pairs. The status is represented as a @see java.lang.Character
     *
     * @return the map
     */
    public Map getChangedPaths();
    
    /**
     * Returns the date of this revision
     *
     * @return the date
     */
    public Date getDate();
    
    /**
     * Returns the commit-message of this revision
     *
     * @return the message
     */
    public String getMessage();
    
    /**
     * Returns the revision number
     *
     * @return the revision number
     */
    public long getRevisionNumber();
    
}
