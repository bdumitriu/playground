package gw.storage.javahl;

import java.util.*;

import gw.storage.*;
import org.tigris.subversion.javahl.*;

/**
 * Wrapper class for the Subversion LogMessage class. A logmessage is tied to a specific commit (revision)
 * and contains all the change information of that revision.
 */
public class SVNStorageLogMessage extends AbstractStorageLogMessage
{
    /**
     * Internal LogMessage object provided by Javahl bindings
     */
    private LogMessage _logMessage;
    
    /**
     * Internal representation of AbstractStorageLogMessage
     */
    private String _author;
    private Date _date;
    private String _message;
    private long _revision;
    
    /**
     * Constructor 
     *
     * @param author Author for the logmessage
     * @param changed A Map containing (path, status) pairs where path is a String and status is a Character
     * @param date Date for the logmessage
     * @param message Message for the logmessage
     * @param revision Revision for the logmessage
     */
    public SVNStorageLogMessage(String author, Date date, String message, long revision)
    {
	    _author = author;
		_date = date;
		_message = message;
		_revision = revision;
    }
    
    public String getAuthor()
    {
        return _author;
    }

    public Date getDate()
    {
        return _date;
    }

    public String getMessage()
    {
        return _message;
    }

    public long getRevisionNumber()
    {
        return _revision;
    }

    /**
     * Constructor for a StorageLogMessage using a javahl object.
     *
     * @param logMessage the LogMessage to encapsulate
     */
    public SVNStorageLogMessage(LogMessage logMessage)
    {
        this(logMessage.getAuthor(), logMessage.getDate(), logMessage.getMessage(), logMessage.getRevisionNumber());
        _logMessage = logMessage;
    }
    
    /**
     * @see StorageLogMessage#getChangedPaths
     */
    public Map getChangedPaths()
    {
        ChangePath[] changePaths = _logMessage.getChangedPaths();

        HashMap paths = new HashMap<String, Character>();

        if (changePaths == null)
            return paths;
        
        for (int i=0; i<changePaths.length; ++i)
        {
            paths.put(changePaths[i].getPath(), new Character(changePaths[i].getAction()));
        }

        return paths;
    }
}
