package gw.storage.csvn.bindings;

import gw.storage.*;
import java.util.*;

public class SVNRAStorageLogMessage extends AbstractStorageLogMessage
{
    /**
     * Constructor for manually building a SVNRAStorageLogMessage. For creating a StorageLogMessage using
     * javahl code:
     *
     * @param author Author for the logmessage
     * @param changed A Map containing (path, status) pairs where path is a String and status is a Character
     * @param date Date for the logmessage
     * @param message Message for the logmessage
     * @param revision Revision for the logmessage
     */
    public SVNRAStorageLogMessage(String author, Map changed, Date date, String message, long revision)
    {
        //super(author, changed, date, message, revision);
    }

	public String getAuthor() {
		// TODO Auto-generated method stub
		return null;
	}

	public Map getChangedPaths() {
		// TODO Auto-generated method stub
		return null;
	}

	public Date getDate() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getMessage() {
		// TODO Auto-generated method stub
		return null;
	}

	public long getRevisionNumber() {
		// TODO Auto-generated method stub
		return 0;
	}
}
