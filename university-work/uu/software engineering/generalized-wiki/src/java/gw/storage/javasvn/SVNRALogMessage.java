package gw.storage.javasvn;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.tmatesoft.svn.core.SVNLogEntry;
import org.tmatesoft.svn.core.SVNLogEntryPath;

import gw.storage.AbstractStorageLogMessage;

/**
 * Custom implementation of StorageLogMessage.
 *
 * @author Bogdan Dumitriu
 */
public class SVNRALogMessage extends AbstractStorageLogMessage
{
    private SVNLogEntry _logEntry;
    private Map<String, Character> changedPaths;

    @SuppressWarnings("unchecked")
	public SVNRALogMessage(SVNLogEntry logEntry)
    {
        _logEntry = logEntry;

        changedPaths = new HashMap<String, Character>();
        Map<String, SVNLogEntryPath> tempPaths = _logEntry.getChangedPaths();
        for (Map.Entry<String, SVNLogEntryPath> entry : tempPaths.entrySet())
        {
        	changedPaths.put(entry.getKey(), entry.getValue().getType());
        }
    }

    public String getAuthor()
    {
        return _logEntry.getAuthor();
    }

    public Map<String, Character> getChangedPaths()
    {
         return changedPaths;
    }

    public Date getDate()
    {
        return _logEntry.getDate();
    }

    public String getMessage()
    {
        return _logEntry.getMessage();
    }

    public long getRevisionNumber()
    {
        return _logEntry.getRevision();
    }
}
