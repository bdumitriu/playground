package gw.storage.javasvn;

import gw.storage.StorageException;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
//import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
//import java.util.Properties;
//import java.util.Set;
//import java.util.StringTokenizer;

//import javax.management.openmbean.OpenDataException;

import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.ISVNEditor;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindow;
import org.tmatesoft.svn.core.io.diff.SVNDiffWindowBuilder;

/**
 * The commit candidate structure. 
 *
 * This class contains all commit information needed for a specific
 * repository path (file or directory). This class allows you to
 * handle all commit situations using its constructors and methods.
 *
 * The commit method can be used to do a commit to the repository.
 * However, this method must be called inside a transaction.
 *
 * A commit item represents all action on a (new) repository path. When doing a commit
 * all items in the CommitItemContainer are committed.
 * We need commit items because the RA layer can access 
 * the same repository path only one time during a svn RA transaction 
 * (this transaction is different from the ones used by the SVNRAStorage!). For example:
 * when you add a new file and want to change properties of that file, you have to this 
 * in a single RA call.
 * 
 * @author Patrick Cloosterman
 * @author Bogdan Dumitriu
 */
public class CommitItem implements Comparable
{
    /** internal flag: indicate the creation of a new dir */
    private final static int ADDDIR  = 1;
    /** internal flag: indicate the creation of a new file */
    private final static int ADDFILE = 2;
    /** internal flag: indicate the deletion of a file or dir */
    private final static int DELETE  = 4;
    /** internal flag: indicate the modification of a file */
    private final static int TEXTMOD = 8;
    /** internal flag: indicate the change of a property of a file or dir */
    private final static int PROPMOD = 16;
    /** internal flag: indicate a copy */
    private final static int ISCOPY  = 32;
    /** internal flag: indicate a move */
    private final static int ISMOVE  = 64;

    /** commit flags for this item */
    private int _flags;

    /** commit path for this item */
    private String _path;

    /** file if this commit adds or modifies a file */
    private File _file;

    /** used for copy or move (path) */
    private String _copyFrom;
    
    /** used for copy or move (path) */
    private String _moveFrom;

    /** the revision the changes are based on */    
    private long _baseRevision = -1;

    /** used for copy or move (revision) */
    private long _copyRev;

    /** changed properties for this item */
    private Map<String, String> _properties;
    
    /** commit item represents a file or directory */    
    private boolean _isDir;

    /**
     * Constructor. Can be used to delete, copy or
     * set properties of an existing file or directory.
     *
     * To add a file or directory or modify file contents, 
     * use an other constructor!
     *
     * @param path the path this commitItem represents
     */
    public CommitItem(boolean isDir, String path)
    {
        _path = path;
        _flags = 0;
        _isDir = isDir;
        _copyFrom = null;
        _copyRev = -1;
    }

    /**
     * Constructor to add a new directory.
     * When isNew is false, this constructor behaves
     * the same as CommitItem(boolean isDir, String path)
     *
     * @param path the path this commitItem represents
     * @param isNew add or modify?
     */
    public CommitItem(boolean isDir, String path, boolean isNew)
    {
        _path = path;
        _flags += (isNew ? ADDDIR : 0);
        _isDir = isDir;
        _copyFrom = null;
        _copyRev = -1;
    }

    /**
     * Constructor used for new and modifying files
     *
     * @param path the path this commitItem represents
     * @param file file contains the filedata
     * @param isNew add or modify?
     */
    public CommitItem(boolean isDir, String path, File file, boolean isNew)
    {
        _path = path;
        _file = file;
        _flags = (isNew ? ADDFILE : TEXTMOD);
        _isDir = isDir;
        _copyFrom = null;
        _copyRev = -1;
    }
    
    /**
     * Set the file to read the data from. This method assumes you already have created this
     * object with the constructor used for new and modified files. Otherwise, calling this
     * method is of no use.
     *
     * @param file file contains the filedata
     */
    public void setFile(File file)
    {
        _file = file;    
    }
    
    public void setPath(String path)
    {
        _path = path;  
    }

    public void setBaseRevision(long revision)
    {
        _baseRevision = revision;
    }
    
    public long getBaseRevision()
    {
        return _baseRevision;
    }

    /**
     * Use this method to add or change a property. To remove a property from
     * a path, use null as the <code>value</code>.
     * 
     * @param name the property name
     * @param value the property value (null to remove property)
     */
    public void setProperty(String name, String value)
    {
        if (_properties == null)
        {
            _properties = new HashMap<String, String>();
        }

        _properties.put(name, value);
        _flags += (isModProperty() ? 0 : PROPMOD);
    }

    /**
     * Use this method to add all the propertys from a map at once.
     *
     * @param name the property name
     * @param value the property value (null to remove property)
     */
    public void setProperty(Map<String, String> propertys)
    {
    	_properties = propertys;
    }

    
    public void markAsAdd()
    {
        if (isFile())
        {
            _flags += (isAddFile() ? 0 : ADDFILE);
        }
        else
        {
            _flags += (isAddDirectory() ? 0 : ADDDIR);
        }
    }

    public void unmarkAsAdd()
    {
        if (isFile())
        {
            _flags -= (isAddFile() ? ADDFILE : 0);
        }
        else
        {
            _flags -= (isAddDirectory() ? ADDDIR : 0);
        }
    }
    
    public void unmarkAsCopy()
    {
        _copyFrom = null;
        _copyRev = -1;
        _flags -= (isCopy() ? ISCOPY : 0);
        unmarkAsAdd();
    }
    
    public void unmarkAsMove()
    {
        _moveFrom = null;
        _flags -= (isCopy() ? ISMOVE : 0);
        unmarkAsCopy();
    }
    
    public void unmarkAsDelete()
    {
        _flags -= (isDelete() ? DELETE : 0);
        
    }

    /**
     * Call this method to indicate that this commit item represents the deletion of
     * a file or directory (depending on what you specified in the constructor of this
     * method).
     */
    public void markAsDelete()
    {
        _flags += (isDelete() ? 0 : DELETE);
    }

    /**
     * Call this method to indicate that this commit item is a copy of another
     * file or directory (depending on what you specified in the constructor of this
     * method).
     * 
     * @param copyFrom the path to the file or directory of which this item is copy
     * @param copyRev the revision of the copied file or directory
     */
    public void markAsCopy(String copyFrom, long copyRev)
    {
        _copyFrom = copyFrom;
        _copyRev = copyRev;
        _flags += (isCopy() ? 0 : ISCOPY);
        _flags += (isDirectory() ? ADDDIR : ADDFILE);
    }
    
    public void markAsMove(String path)
    {
        _flags += (isMove() ? 0 : ISMOVE);
        _moveFrom = path;
    }

    /**
     * Returns the path of this item.
     */
    public String getPath()
    {
        return _path;
    }
    
    /**
     * Returns the file whose contents represents the data added/changed by this item.
     */
    public File getFile()
    {
        return _file;
    }    

    /**
     * Returns the list of properties added/changed/deleted by this item.
     */
    public Map<String, String> getProperties()
    {
        return _properties;
    }

    /**
     * Returns the path of the file/directory this item is a copy of. If this
     * item is not a copy, then null is returned. 
     */
    public String getCopyFromPath()
    {
        return _copyFrom;
    }
    
    public String getMoveFromPath()
    {
        return _moveFrom;
    }

    /**
     * Returns the revision of the file/directory this item is a copy of. If this
     * item is not a copy, then -1 is returned. 
     */
    public long getCopyRevision()
    {
        return _copyRev;
    }

    /**
     * Returns true if this item represents a directory.
     */
    public boolean isDirectory()
    {
        return _isDir;
    }
    
    /**
     * Returns true if this item represents a file.
     */
    public boolean isFile()
    {
        return (!_isDir);
    }

    /**
     * Returns true if this item represents the deletion of a file or of a directory.
     */
    public boolean isDelete()
    {
        return ((_flags & DELETE) == DELETE);
    }

    /**
     * Returns true if this item represents the deletion of a directory.
     */
    public boolean isDeleteDirectory()
    {
        return isDelete() && isDirectory();
    }

    /**
     * Returns true if this item represents the deletion of a file.
     */
    public boolean isDeleteFile()
    {
        return isDelete() && isFile();
    }

    /**
     * Returns true if this item represents the addition of a file or of a directory.
     */
    public boolean isAdd()
    {
        return isAddFile() || isAddDirectory();
    }
    
    /**
     * Returns true if this item represents the addition of a directory.
     */
    public boolean isAddDirectory()
    {
        return ((_flags & ADDDIR) == ADDDIR);
    }

    /**
     * Returns true if this item represents the addition of a file.
     */
    public boolean isAddFile()
    {
        return ((_flags & ADDFILE) == ADDFILE);
    }

    /**
     * Returns true if this item represents the copy of a file or of a directory.
     */
    public boolean isCopy()
    {
        return ((_flags & ISCOPY) == ISCOPY);
    }
    
    public boolean isMove()
    {
        return ((_flags & ISMOVE) == ISMOVE);
    }

    /**
     * Returns true if this item represents the copy of a directory.
     */
    public boolean isCopyDirectory()
    {
        return isCopy() && isDirectory();
    }

    /**
     * Returns true if this item represents the copy of a file.
     */
    public boolean isCopyFile()
    {
        return isCopy() && isFile();
    }

    /**
     * Returns true if this item modifies any properties of the path.
     */
    public boolean isModProperty()
    {
        return ((_flags & PROPMOD) == PROPMOD);
    }

    /**
     * Returns true if this item represents the modification of an already existing
     * file.
     */
    public boolean isModFile()
    {
        return ((_flags & TEXTMOD) == TEXTMOD);
    }
    
    /**
     * Commits all actions of this commit item. The editor that will
     * be used to commit the file is expected to have had its openRoot(...)
     * method already called.
     * <br /><br />
     * This method will not close the editor so that it can be reused.
     */
    public void commit(ISVNEditor editor) throws SVNException, StorageException
    {
        if (isDelete())
        {
            editor.deleteEntry(getPath(), getBaseRevision());
        }

        if (isAddDirectory() || (isModProperty() && isDirectory()))
        {
            if (_path.equals(""))
            {
                editor.openRoot(getBaseRevision());
                //System.out.println("openRoot(" + getBaseRevision() + ")");
            }
            else
            {
                if (isAddDirectory())
                {
                    editor.addDir(getPath(), getCopyFromPath(), getCopyRevision());
                    //System.out.println("addDir(" + getPath() + ", " + getCopyFromPath() + ", " + getCopyRevision() + ")");
                }
                else
                {
                    editor.openDir(getPath(), getBaseRevision());
                    //System.out.println("openDir(" + getPath() + ", " + getBaseRevision() + ")");
                }
            }

            if (isModProperty())
            {
            	    for (Map.Entry<String, String> entry : _properties.entrySet())
                {
            	        editor.changeDirProperty(entry.getKey(), entry.getValue());
                }
            }
        }

//        if (isCopy())
//        {
//            editor.addFile(getPath(), getCopyFromPath(), getCopyRevision());
//            editor.closeFile(getPath(), null);
//        }

        if (isAddFile() || isModFile() || (isModProperty() && isFile()))
        {
            if (isAddFile())
            {
                editor.addFile(getPath(), getCopyFromPath(), getCopyRevision());
                //System.out.println("addFile(" + getPath() + ", " + getCopyFromPath() + ", " + getCopyRevision() + ")");
            }
            else    // isModFile() or (isFile() && isModProperty())
            {
                editor.openFile(getPath(), getBaseRevision());
                //System.out.println("openFile(" + getPath() + ", " + getBaseRevision() + ")");
            }

            if (isModProperty())
            {
                for (Map.Entry<String, String> entry : _properties.entrySet())
                {
                    editor.changeFileProperty(getPath(), entry.getKey(), entry.getValue());
                }
            }

            if (_file != null)
            {
                // prepare the file for applying the delta
                // TODO: maybe compute base checksum here, although it doesn't seem necessary
                //       since it works without it as well
                editor.applyTextDelta(getPath(), null);
    
                // obtain the OutputStream to write the contents of the file to 
                long deltaLength = _file.length();
                SVNDiffWindow diffWindow = SVNDiffWindowBuilder.createReplacementDiffWindow(deltaLength);
                OutputStream os = editor.textDeltaChunk(getPath(), diffWindow);

                try
                {
                    // open _file for reading
                    BufferedInputStream fis = new BufferedInputStream(new FileInputStream(_file));

                    // read from _file and write to the OutputStream
                    byte[] buffer = new byte[1024];
                    int nrBytes;
                    while ((nrBytes = fis.read(buffer, 0, 1024)) != -1)
                    {
                        os.write(buffer, 0, nrBytes);
                    }
    
                    os.close();
                }
                catch (IOException e)
                {
                    editor.abortEdit();
                    throw new StorageException(e.getMessage());
                }
    
                // mark the end of applying the delta
                editor.textDeltaEnd(getPath());
            }
            else if ((isAddFile() || isModFile()) && !isCopy())
            {
                editor.applyTextDelta(getPath(), null);
                SVNDiffWindow diffWindow = SVNDiffWindowBuilder.createReplacementDiffWindow(0);
                OutputStream os = editor.textDeltaChunk(getPath(), diffWindow);
                try
                {
                    os.close();
                }
                catch (IOException e)
                {
                    editor.abortEdit();
                    throw new StorageException(e.getMessage());
                }
                editor.textDeltaEnd(getPath());
            }

            // close the added file so that changes are fixed
            editor.closeFile(getPath(), null);
            //System.out.println("closeFile()");
        }
    }

    /**
     * In order to work inside a directory, openDir has to be called successively
     * for all the intermediate directories. For example, to add the file dir1/dir2/dir3/file
     * openDir has to be called 3 times, for opening dir1, then dir1/dir2 and then
     * dir1/dir2/dir3. This is what this method does, for whatever _path this
     * commit item has.
     * 
     *  TODO: refine the implementation to take less time, if possible 
     */
    private void openPath(ISVNEditor editor) throws SVNException
    {
        String cleanPath = getPath();

        // remove a leading slash, if any
        if (cleanPath.startsWith("/"))
        {
            cleanPath = getPath().substring(1);
        }

        // remove a trailing slash, if any
        if (cleanPath.endsWith("/"))
        {
            cleanPath = cleanPath.substring(0, cleanPath.length() - 1);
        }

        // call openDir for each path element (except the last)
        String current = "";
        int index;
        while ((index = cleanPath.indexOf('/')) != -1)
        {
            if (current.equals(""))
            {
                current += cleanPath.substring(0, index);
            }
            else
            {
                current += "/" + cleanPath.substring(0, index);
            }

            cleanPath = cleanPath.substring(index + 1);
            
            editor.openDir(current, getBaseRevision());
        }
    }

    /**
     * All the directories opened by openPath have to be closed as well. So,
     * this method simply calls closeDir the exact number of times which is
     * needed for the _path of this commit item.
     */
    private void closePath(ISVNEditor editor) throws SVNException
    {
        String cleanPath = getPath();

        // remove a leading slash, if any
        if (cleanPath.startsWith("/"))
        {
            cleanPath = getPath().substring(1);
        }

        // remove a trailing slash, if any
        if (cleanPath.endsWith("/"))
        {
            cleanPath = cleanPath.substring(0, cleanPath.length() - 1);
        }
        
        for (int i = 0; i < cleanPath.length(); i++)
        {
            if (cleanPath.charAt(i) == '/')
            {
                editor.closeDir();
            }
        }
    }

    public int compareTo(Object o)
    {
        CommitItem ci = (CommitItem) o;
        return _path.compareTo(ci._path);
    }
    
    @Override
    public String toString()
    {
        String result;
        if (isDelete() && !isAdd())
        {
            result = "Delete ";
        }
        else if (isDelete() && isAdd())
        {
            result = "Delete and readd ";
        }
        else if (!isDelete() && isAdd())
        {
            result = "Add ";
        }
        else
        {
            result = "Modify ";
        }

        if (isModFile() && isModProperty())
        {
            result += "contents and properties of ";
        }
        else if (isModFile())
        {
            result += "contents of ";
        }
        else if (isModProperty())
        {
            result += "properties of ";
        }

        if (isFile())
        {
            result += "file ";
        }
        else
        {
            result += "directory ";
        }

        result += _path;

        if (isCopy())
        {
            result += " as copy of " + getCopyFromPath() + "@" + getCopyRevision();
        }

        return result;
    }
    
    /*protected Object clone() throws CloneNotSupportedException
    {
        CommitItem clone = (CommitItem) super.clone();
        return clone;
    }*/
}
