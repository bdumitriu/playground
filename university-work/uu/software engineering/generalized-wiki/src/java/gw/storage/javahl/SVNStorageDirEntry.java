package gw.storage.javahl;

import java.util.*;
import gw.storage.*;
import org.tigris.subversion.javahl.*;

public class SVNStorageDirEntry extends StorageDirEntry
{
    /**
     * Internal representation of a StorageDirEntry, using a javahl object
     */
    private DirEntry _dirEntry;
    
    /**
     * Constructor for a StorageDirEntry using a javahl object.
     *
     * @param dirEntry Subversion javahl object containing the node's dir. entry
     * 
     */
    public SVNStorageDirEntry(DirEntry dirEntry)
    {
        _dirEntry = dirEntry;
    }

    /**
     * @see StorageDirEntry#getLastChangedDate
     */
    public Date getLastChangedDate()
    {
        return _dirEntry.getLastChanged();
    }

    /**
     * @see StorageDirEntry#getLastChangedRevision
     */
    public long getLastChangedRevision()
    {
        return _dirEntry.getLastChangedRevisionNumber();
    }

    /**
     * @see StorageDirEntry#getLastChangedAuthor
     */
    public String getLastChangedAuthor()
    {
        return _dirEntry.getLastAuthor();
    }


    /**
     * @see StorageDirEntry#isDirectory
     */
    public boolean isDirectory()
    {
        if(NodeKind.getNodeKindName(_dirEntry.getNodeKind()).equalsIgnoreCase("dir"))
        {
            return true;
        }

        return false;

    }

    /**
     * @see StorageDirEntry#isFile
     */
    public boolean isFile()
    {
        if(NodeKind.getNodeKindName(_dirEntry.getNodeKind()).equalsIgnoreCase("file"))
        {
            return true;
        }

        return false;

    }
}
