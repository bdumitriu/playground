package gw.storage.csvn;

import gw.storage.*;
import gw.storage.csvn.bindings.*;
import gw.storage.javahl.SVNStorageException;
import java.io.*;
import java.util.*;


/**
 * A class implementing the Storage interface with native Subversion RA bindings.
 * 
 */
public class SVNRAStorage extends AbstractStorage
{
    public final static int HEAD_REVISION = -1;
    private String _repository;
    private String _username;
    private String _password;
    private SVNJNIAdapter _ralib;

    ArrayList _actions;
    
    /**
     * Constructor
     *
     * @param repository Complete URL to the repository containing the GWiki
     * @param username The username for whom this SVNRAStorage is constructed, also used for auth. purposes
     * @param password Corresponding password for username
     */    
    public SVNRAStorage(String repository, String username, String password) throws StorageException
    {
        _actions = new ArrayList();
        
        _repository = repository;
       
        _username = username;
        _password = password;

        try
        {
            _ralib = new SVNJNIAdapter(_username, _password, repository);
        } catch (RAException rae)
        {
            throw new SVNStorageException("Unable to open an RA session: " + rae.getMessage());
        }
    }
   
    public Iterator blame(String path,
                          long revisionStart,
                          long revisionEnd) throws StorageException
    {
        Iterator it = null;
        try
        {
            List list = _ralib.blame(path, revisionStart, revisionEnd, _ralib._sessionPointer);
            it = list.iterator();
        }
        catch (RAException rae)
        {
            throw new SVNStorageException("Unable to get blame: " + rae.getMessage());
        }

        return it;
    }    
       
    public OutputStream storeFile(String path) throws StorageException
    {
        path = normalizePath(path);
        SVNRAOutputStream stream;

        try
        {
            File file = File.createTempFile("gw-svnrastorage", null);
            stream = new SVNRAOutputStream(this, file, path, fileExists(path));
        }
        catch (IOException ex)
        {
            throw new SVNStorageException("Unable to store file: " + ex.getMessage());
        }

        return stream;
    }
    
    public InputStream getFile(String path) throws StorageException
    {
        return this.getFile(path, SVNJNIAdapter.HEAD_REVISION);
    }
    
    public InputStream getFile(String path, long revision) throws StorageException
    {
        //System.out.println("SVNRAStorage: retrieving file `" + path + "'");
        InputStream stream = null;

        if (path.charAt(0) == '/')
        {
            path = path.substring(1);
        }

        try
        {
            byte[] contents = _ralib.getFile(path, revision, _ralib._sessionPointer);
            stream = new ByteArrayInputStream(contents);
        }
        catch (Exception ex)
        {
            throw new StorageException("Unable to get file: " + ex.getMessage());
        }
        
       return stream;
    }
    
    public Iterator getDirListing(String path) throws StorageException
    {
        return getDirListing(path, false);
    }
    
    public Iterator getDirListing(String path, boolean recurse) throws StorageException
    {
        path = normalizePath(path);
        List listing = null;
            
        try
        {
            listing = _ralib.getDirListing(path, SVNJNIAdapter.HEAD_REVISION, recurse, _ralib._sessionPointer);
        }
        catch (RAException rae)
        {
            throw new SVNStorageException("Unable to get dir listing: " + rae.getMessage());
        }

        return listing.iterator();
    }

    public Map getDirListing(String path, long revision, boolean recurse) throws StorageException
    {
        // todo:
        // write a SVNRAStorageDirEntry class
        // to create such a object, you need to get the properties of the path
        // and add them to the SVNRAStorageDirEntry object.
        throw new SVNStorageException("Not implemented.");
    }

    public Map getStatus(String path, boolean recursive) throws SVNStorageException
    {
        // get a dirlisting and use _actions to check what the status is
        throw new SVNStorageException("Not implemented.");
    }

    public Map getStatus(String path) throws SVNStorageException
    {
        return getStatus(path, false);
    }
    
    public boolean fileExists(String path) throws StorageException
    {
        path = normalizePath(path);
        try
        {
            return _ralib.fileExists(path, _ralib._sessionPointer);
        } catch (RAException rae)
        {
            throw new SVNStorageException("fileExists: " + rae.getMessage());
        }
    }

    private String normalizePath(String path)
    {
        if ((path == null) || (path.equals("")))
            return path;
        
        // Chop off leading slash
        if (path.charAt(0) == '/')
        {
            path = path.substring(1);
        }

        // Chop off trailing slash
        if (path.endsWith("/")) {
            path = path.substring(0, path.length()-1);  
        }

        return path;
    }

    public boolean isDirectory(String path) throws StorageException
    {
        path = normalizePath(path);
        try
        {
            return _ralib.isDirectory(path, _ralib._sessionPointer);
        } catch (RAException rae)
        {
            throw new SVNStorageException("isDirectory: " + rae.getMessage());
        }
    }
    
    public void makeDirectory(String pathName) throws StorageException
    {
        if (fileExists(pathName) || isDirectory(pathName)) {
            throw new SVNStorageException("Unable to create directory: directory or file already exists");
        }

        SVNRAAction action = new SVNRAAction();
        action.type = SVNRAAction.ADDDIR;
        action.isDirectory = true;
        action.path1 = pathName;
        _actions.add(action);
    }
    
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException
    {
        if(!fileExists(oldPath)) {
            throw new SVNStorageException("Unable to move file: source file does not exist");
        }
        if(fileExists(newPath)) {
            throw new SVNStorageException("Unable to move file: target file already exists");
        }
        SVNRAAction action = new SVNRAAction();
        action.type = SVNRAAction.MOVE;
        action.path1 = oldPath;
        action.path2 = newPath;
        _actions.add(action);
    }

    public void copyFile(String originalPath, String copiedPath) throws StorageException
    {
        if(!fileExists(originalPath)) {
            throw new SVNStorageException("Unable to copy file: source file does not exist");
        }
        if(fileExists(copiedPath)) {
            throw new SVNStorageException("Unable to copy file: target file already exists");
        }
        SVNRAAction action = new SVNRAAction();
        action.type = SVNRAAction.COPY;
        action.path1 = originalPath;
        action.path2 = copiedPath;
        _actions.add(action);
    }
    
    public void deleteFile(String path, boolean force) throws StorageException
    {
        if(!fileExists(path)) {
            throw new SVNStorageException("Unable to delete file: target file does not exist");
        }
        SVNRAAction action = new SVNRAAction();
        action.type = SVNRAAction.DELETE;
        action.path1 = path;
        _actions.add(action);
    }
          
    public void revertFile(String path) throws StorageException
    {
        throw new SVNStorageException("Not implemented.");
    }
          
    public void revertFile(String path, long revision) throws StorageException
    {
        throw new SVNStorageException("Not implemented.");
    }
    
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException
    {
        throw new SVNStorageException("Not implemented.");
    }
    
    public SortedSet getLog(String path) throws StorageException
    {
        SortedSet result = null;
        
        if (path.charAt(0) == '/')
        {
            path = path.substring(1);
        }        
        
        try
        {
            result = _ralib.getLog(path, _ralib._sessionPointer);
        }
        catch (RAException rae)
        {
            throw new StorageException("unable to get log: " + rae.getMessage());
        }
        return result;        
    }
    
    public void setProperty(String path, String property, String value, boolean recurse) throws StorageException
    {
        SVNRAAction action = new SVNRAAction();
        action.type = SVNRAAction.SETPROP;
        action.path1 = normalizePath(path);
        action.propname = property;
        action.propvalue = value;
        action.recurse = recurse;
        _actions.add(action);
    }
    
    public Map getProperties(String path) throws StorageException
    {
        path = normalizePath(path);
        try
        {
            return _ralib.getProperties(path, SVNJNIAdapter.HEAD_REVISION, _ralib._sessionPointer);
        } catch (RAException rae)
        {
            throw new SVNStorageException("Unable to get properties for " + path + ": " + rae.getMessage());
        }
    }

    public boolean commit(String message) throws StorageException
    {
        if (_actions.isEmpty())
        {
            return false; // Nothing to commit.
        }

        // Start the transaction in the native code
        try
        {
            _ralib.transactionStart(message, _ralib._sessionPointer);
        } catch (RAException rae)
        {
            throw new SVNStorageException("Commit failed: " + rae.getMessage());
        }
        
        // Iterate over the actions and perform them all.
        String name;
        Iterator iter = _actions.iterator();
        while(iter.hasNext())
        {
            try
            {
                SVNRAAction action = (SVNRAAction) iter.next();
                switch(action.type)
                {
                    case SVNRAAction.MOVE:
                          System.out.println("no move supported yet");
//                        System.out.println("* Moving " + action.path1 + " to " + action.path2);
//                        _ralib.transactionCopyFile(action.path2, action.path1, _ralib._sessionPointer);
//                        _ralib.transactionRemoveEntry(action.path1, _ralib._sessionPointer);
                        break;
                    case SVNRAAction.ADD: // tested ok
                        System.out.println("* Adding " + action.path1);
                        _ralib.transactionSendFile(action.path1, action.file.getPath(), false, _ralib._sessionPointer);
                        break;
                    case SVNRAAction.ADDDIR: // tested ok, no traversing needed here
                        System.out.println("* Adding " + action.path1);
                        _ralib.transactionAddDirectory(action.path1, _ralib._sessionPointer);
                        break;
                    case SVNRAAction.DELETE: // tested ok
                        System.out.println("* Removing " + action.path1);
                        _ralib.transactionEnterDirectory(action.path1, _ralib._sessionPointer);
                        _ralib.transactionRemoveEntry(action.path1, _ralib._sessionPointer);
                        _ralib.transactionLeaveDirectory(_ralib._sessionPointer);
                        break;
                    case SVNRAAction.MODIFY: // tested ok
                        System.out.println("* Modifying " + action.path1);
                        System.out.println("* source " + action.file.getPath());                        
                        _ralib.transactionSendFile(action.path1, action.file.getPath(), true, _ralib._sessionPointer);
                        break;
                    case SVNRAAction.COPY:
                          System.out.println("no copy supported yet");
                          // transactionCopyFile needs to create a tempfile (of the sourcefilename)
//                        System.out.println("* Copying " + action.path1 + " to " + action.path2);
//                        _ralib.transactionCopyFile(action.path2, action.path1, _ralib._sessionPointer);
                        break;
                    case SVNRAAction.SETPROP: // tested ok
/*                    this code works, but SaveFile.java tries to set properties of a new file (->this fails)
                       so the commit fails
                       
                       problem: how to set properties of a file that is added in the same commit?
                       solution?: svn_delta.h says: use the same baton as used for adding the file
                                  so, - transactionSendFile should also set the properties?  apr_hash_t *properties,
                                      - or return a baton for that file 
                                      - or use a java calllback method that can set the properties during the sendfile 
                                      
                                      >>>> i think the first option is the easiest one!!!
                       
                       svn_delta.h:
                                  When the producer calls @c open_file or @c add_file, either:
                        528  * 
                        529  *    (a) The producer must follow with the changes to the file
                        530  *    (@c change_file_prop and/or @c apply_textdelta, as applicable)
                        531  *    followed by a @c close_file call, before issuing any other file
                        532  *    or directory calls, or
                        533  *
                        534  *    (b) The producer must follow with a @c change_file_prop call if
                        535  *    it is applicable, before issuing any other file or directory
                        536  *    calls; later, after all directory batons including the root
                        537  *    have been closed, the producer must issue @c apply_textdelta
                        538  *    and @c close_file calls.                       
                       */
                        System.out.println("setting prop of path: "+action.path1);
                        if (_ralib.isDirectory(action.path1, _ralib._sessionPointer))
                        {
                            _ralib.transactionEnterDirectory(action.path1, _ralib._sessionPointer);
                            _ralib.transactionChangeDirProperty(action.propname, action.propvalue, _ralib._sessionPointer);
                            _ralib.transactionLeaveDirectory(_ralib._sessionPointer);
                        }/*
                        else
                            _ralib.transactionChangeFileProperty(action.path1, action.propname, action.propvalue, 
                                                                 _ralib._sessionPointer);*/
                        
                        break;
                }
            } catch (Exception rae) {
                try
                {
                    // Abort the transaction to clean up
                    _ralib.transactionAbort(_ralib._sessionPointer);
                } catch (RAException rae2) {}
                
                throw new SVNStorageException("Unable to commit: " + rae.getMessage());
            }

        }

        // Signal the native code that we are finished with the commit
        try
        {
            _ralib.transactionCommit(_ralib._sessionPointer);
        } catch (RAException rae)
        {
            throw new SVNStorageException("Unable to commit: " + rae.getMessage());
        }

        return true;
    }

    public void update() throws StorageException
    {
        _ralib.update(_ralib._sessionPointer);
    }

    public Iterator getConflicts() throws StorageException
    {
        // TODO: implement this. Currently just returns an empty list.
        ArrayList list = new ArrayList();
        return list.iterator();
    }

    public void setResolved(String path) throws StorageException
    {
        throw new SVNStorageException("Not implemented."); 
    }
    
    public void cancelTransaction() throws StorageException
    {
        // TODO: delete all temporary files.
        _actions = new ArrayList();
    }

    public void beginTransaction() throws StorageException
    {
        _actions = new ArrayList();
    }
    
    public void endTransaction(String message) throws StorageException
    {
        commit(message);
    }

    
    /**
     * Returns the path/URL to the repository
     */
    protected String getRepository()
    {
        return _repository;
    }

    public void setUsername(String username) {
        _username = username;
    }
}
