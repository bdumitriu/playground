package gw.storage.javahl;

import java.util.*;
import gw.storage.*;
import org.tigris.subversion.javahl.*;

/**
 * Wrapper class for the Subversion Status class
 */
public class SVNStorageStatusMessage extends StorageStatusMessage
{
    /**
     * Internal Status object
     */
    private Status _SVNStatus;

    /**
     * Constructor
     *
     * @param SVNStatus the provided Status object
     */
    public SVNStorageStatusMessage(Status SVNStatus)
    {
        _SVNStatus = SVNStatus;
    }

    /**
     * @see StorageStatusMessage#getLastChangedDate
     */
    public Date getLastChangedDate()
    {
        return _SVNStatus.getLastChangedDate();
    }

    /**
     * @see StorageStatusMessage#getLastChangedRevision
     */
    public long getLastChangedRevision()
    {
        return _SVNStatus.getLastChangedRevisionNumber();
    }

    /**
     * @see StorageStatusMessage#getLastCommitAuthor
     */
    public String getLastCommitAuthor()
    {
        return _SVNStatus.getLastCommitAuthor();
    }

    /**
     * @see StorageStatusMessage#getRevision
     */
    public long getRevision()
    {
        return _SVNStatus.getRevisionNumber();
    }
    
    /**
     * @see StorageStatusMessage#isAdded
     */
    public boolean isAdded()
    {
        return _SVNStatus.isAdded();
    }
    
    /**
     * @see StorageStatusMessage#isCopied
     */
    public boolean isCopied()
    {
        return _SVNStatus.isCopied();
    }

    /**
     * @see StorageStatusMessage#isDeleted
     */
    public boolean isDeleted()
    {
        return _SVNStatus.isDeleted();
    }

    /**
     * @see StorageStatusMessage#isModified
     */
    public boolean isModified()
    {
        return _SVNStatus.isModified();
    }

    /**
     * @see StorageStatusMessage#isDirectory
     */
    public boolean isDirectory()
    {

        if(NodeKind.getNodeKindName(_SVNStatus.getNodeKind()).trim().equalsIgnoreCase("dir"))
        {
            return true;
        }

        return false;
      
    }

    /**
     * @see StorageStatusMessage#isFile
     */
    public boolean isFile()
    {
        if(NodeKind.getNodeKindName(_SVNStatus.getNodeKind()).equalsIgnoreCase("file"))
        {
            return true;
        }

        return false;

    }
}
