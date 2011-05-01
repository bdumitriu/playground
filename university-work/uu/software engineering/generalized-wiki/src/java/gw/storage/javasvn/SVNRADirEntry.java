/**
 * 
 */
package gw.storage.javasvn;

import java.util.Date;

import org.tmatesoft.svn.core.SVNDirEntry;
import org.tmatesoft.svn.core.SVNNodeKind;

import gw.storage.StorageDirEntry;

/**
 * Custom implementation of StorageDirEntry.
 * 
 * TODO: implement getLastChangedDate().
 * 
 * @author Bogdan Dumitriu
 */
public class SVNRADirEntry extends StorageDirEntry
{
    /** The path represented by this entry */
    private String _path;

    private SVNDirEntry _entry;

    public SVNRADirEntry(String basePath, SVNDirEntry entry)
    {
        _path = basePath + entry.getName();
        _entry = entry;
    }

    /**
     * @see gw.storage.StorageDirEntry#getLastChangedDate()
     */
    public Date getLastChangedDate()
    {
        return _entry.getDate();
    }

    /**
     * @see gw.storage.StorageDirEntry#getLastChangedRevision()
     */
    public long getLastChangedRevision()
    {
        return _entry.getRevision();
    }

    /**
     * @see gw.storage.StorageDirEntry#getLastChangedAuthor()
     */
    public String getLastChangedAuthor()
    {
        return _entry.getAuthor();
    }

    /**
     * @see gw.storage.StorageDirEntry#isDirectory()
     */
    public boolean isDirectory()
    {
        return (_entry.getKind() == SVNNodeKind.DIR);
    }

    /**
     * @see gw.storage.StorageDirEntry#isFile()
     */
    public boolean isFile()
    {
        return (_entry.getKind() == SVNNodeKind.FILE);
    }

    public String getPath()
    {
        return _path;
    }
}
