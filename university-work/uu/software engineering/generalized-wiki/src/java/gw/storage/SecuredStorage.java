package gw.storage;


import gw.users.*;
import gw.users.acl.*;
import java.util.*;
import java.io.*;


/**
 * The secured storage is a decorator for a given storage. It adds
 * security checks to some of the storage methods.
 * 
 * In general, given a path to a certain resource, the wrapper checks
 * if there is hierarchical access to this path. This means that there
 * is browse persmission assigned to all parent directories starting at
 * the root. Depending on the operation, it will add some additional
 * checks such as requiring read permission on directories to view the
 * files in the directory, or read/write access when viewing/editing a
 * file.
 * 
 * In some cases where the delegate storage returns a list or a map, the
 * contents of these maps are filter filtered by requiring some
 * permissions to be present on the paths in the output. Specifically,
 * the password property is never included in the output of a directory
 * listing.
 */
public class SecuredStorage extends DelegateStorage {
    private static final String PASSWORD_PROPNAME = "password";
    
    protected User _owner;
    
    
    /**
     * Initializes an instance of the secured storage. This storage wraps
     * around the given delegate storage. The owner is used to check permissions.
     * 
     * @param delegate The storage delegate.
     * @param owner  The owner of this Storage.
     */
	public SecuredStorage(Storage delegate, User owner) {
		super(delegate);
        
        _owner = owner;
	}
    
    /**
     * Checks hierarchical access and permission for blame action.
     * @see gw.storage.Storage#blame(java.lang.String, long, long)
     */
    public Iterator<StorageBlameLine> blame(String path, long revisionStart, long revisionEnd) throws StorageException {
        checkHierarchicalTraverseAccess(path);        
        checkFlatAccess(path, GwACLResourceActions.BLAME_ACTION);
        
        return _delegate.blame(path, revisionStart, revisionEnd);
    }
    
    /**
     * Checks hierarchical access and permission for edit action.
     * @see gw.storage.Storage#storeFile(java.lang.String)
     */
    public OutputStream storeFile(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.EDIT_ACTION);
        
        return _delegate.storeFile(path);
    }
    
    /**
     * Checks hierarchical access and permission for retrieve action.
     * @see gw.storage.Storage#getFile(java.lang.String)
     */
    public InputStream getFile(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.RETRIEVE_ACTION);
        
        return _delegate.getFile(path);
    }
    
    /**
     * Checks hierarchical access and permission for browse action.
     * @see gw.storage.Storage#getDirListing(java.lang.String)
     */
    public Iterator<String> getDirListing(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.BROWSE_ACTION);
        
        return postFilterDirListing(_delegate.getDirListing(path));
    }

    /**
     * Checks hierarchical access and permission for browse action.
     * @see gw.storage.Storage#getDirListing(java.lang.String, boolean)
     */
    public Iterator<String> getDirListing(String path, boolean recurse) throws StorageException {
              
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.BROWSE_ACTION);
        
        return postFilterDirListing(_delegate.getDirListing(path, recurse));
    }

    /**
     * Checks hierarchical access and permission for browse action.
     * @see gw.storage.Storage#getDirListing(java.lang.String, long, boolean)
     */
    public Map<String, StorageDirEntry> getDirListing(String path, long revision, boolean recurse) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.BROWSE_ACTION);
        
        return postFilterPathMap(_delegate.getDirListing(path, revision, recurse));
    }
    
    /**
     * Checks hierarchical access and permission for browse action.
     * @see gw.storage.Storage#getStatus(java.lang.String)
     */
    public Map<String, StorageStatusMessage> getStatus(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.RETRIEVE_ACTION);
        
        return postFilterPathMap(_delegate.getStatus(path));
    }

    /**
     * Checks hierarchical access and permission for browse action.
     * @see gw.storage.Storage#getStatus(java.lang.String, boolean)
     */
    public Map<String, StorageStatusMessage> getStatus(String path, boolean recursive) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.RETRIEVE_ACTION);
        
        return postFilterPathMap(_delegate.getStatus(path, recursive));
    }
    
    /**
     * Checks hierarchical access to the file.
     * @see gw.storage.Storage#fileExists(java.lang.String)
     */
    public boolean fileExists(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        
        return _delegate.fileExists(path);
    }

    /**
     * Check hierarchical access to the path.
     * @see gw.storage.Storage#isDirectory(java.lang.String)
     */
    public boolean isDirectory(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        
        return _delegate.isDirectory(path);
    }
    
    /**
     * Checks hierarchical access and permission for create directory
     * action on the parent directory.
     * @see gw.storage.Storage#makeDirectory(java.lang.String)
     */
    public void makeDirectory(String pathName) throws StorageException {
        checkHierarchicalTraverseAccess(pathName);
        
        FilePathACLResource resource = new FilePathACLResource(pathName);
        checkFlatAccess(resource.getParentResource().getIdentifier(), GwACLResourceActions.CREATE_DIRECTORY_ACTION);
        
        _delegate.makeDirectory(pathName);
    }

    /**
     * Checks hierarchical access on old and new path, move action on the old file and
     * either create permission on the parent directory if the new file does not exist,
     * or edit permission if it does exist.
     * @see gw.storage.Storage#moveFile(java.lang.String, java.lang.String, boolean)
     */
    public void moveFile(String oldPath, String newPath, boolean force) throws StorageException {
        checkHierarchicalTraverseAccess(oldPath);
        checkHierarchicalTraverseAccess(newPath);
        
        checkFlatAccess(oldPath, GwACLResourceActions.MOVE_ACTION);
        checkFlatAccess(new FilePathACLResource(newPath).getParentResource().getIdentifier(), GwACLResourceActions.CREATE_ACTION);
        
        _delegate.moveFile(oldPath, newPath, force);
    }

    /**
     * Checks hierarchical access on old and new path, copy action on the old file and
     * either create permission on the parent directory if the new file does not exist,
     * or edit permission if it does exist.
     * @see gw.storage.Storage#copyFile(java.lang.String, java.lang.String)
     */
    public void copyFile(String originalPath, String copiedPath) throws StorageException {
        checkHierarchicalTraverseAccess(originalPath);
        checkHierarchicalTraverseAccess(copiedPath);

        checkFlatAccess(originalPath, GwACLResourceActions.COPY_ACTION);
        
        if (_delegate.fileExists(copiedPath))
            checkFlatAccess(copiedPath, GwACLResourceActions.EDIT_ACTION);
        else
            checkFlatAccess(new FilePathACLResource(copiedPath).getParentResource().getIdentifier(), GwACLResourceActions.CREATE_ACTION);
                
        _delegate.copyFile(originalPath, copiedPath);
    }

    /**
     * Checks hierarchical access on the path and delete action on the file.
     * @see gw.storage.Storage#deleteFile(java.lang.String, boolean)
     */
    public void deleteFile(String path, boolean force) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.DELETE_ACTION);
        
       _delegate.deleteFile(path, force);
    }

    /**
     * Checks hierarchical access on the path and diff action on the file.
     * @see gw.storage.Storage#getFileDiff(java.lang.String, long, long)
     */
    public String getFileDiff(String path, long revision1, long revision2) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.DIFF_ACTION);
        
        return _delegate.getFileDiff(path, revision1, revision2);
    }

    /**
     * Checks hierarchical access on the path and log action on the file.
     * @see gw.storage.Storage#getLog(java.lang.String)
     */
    public SortedSet<StorageLogMessage> getLog(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.LOG_ACTION);
        
        return _delegate.getLog(path);
    }

    /**
     * Checks hierarchical access on the path. If the property is
     * an acl prefix, EDIT_ACL_ACTION is required, if the property is
     * a password, then password is required. If the property is none
     * of these, then property action is required.
     * @see gw.storage.Storage#setProperty(java.lang.String, java.lang.String, java.lang.String, boolean)
     */
    public void setProperty(String path, String property, String value, boolean recurse) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        
        if (property.toLowerCase().startsWith(GwACLPermissions.ACL_PREFIX))
            checkFlatAccess(path, GwACLResourceActions.EDIT_ACL_ACTION);
        else if (property.toLowerCase().startsWith(PASSWORD_PROPNAME))
            checkFlatAccess(path, GwACLResourceActions.EDIT_PASSWORD_ACTION);
        else
            checkFlatAccess(path, GwACLResourceActions.EDIT_PROPERTY_ACTION);
        
        _delegate.setProperty(path, property, value, recurse);
    }

    /**
     * Checks hierarchical access on the path. Also checks if the
     * retrieve property actions is possible on the file. The
     * returned properties are filtered to ensure it does not
     * contain a password.
     * @see gw.storage.Storage#getProperties(java.lang.String)
     */
    public Map<String, String> getProperties(String path) throws StorageException {
        checkHierarchicalTraverseAccess(path);
        checkFlatAccess(path, GwACLResourceActions.RETRIEVE_PROPERTY_ACTION);
        
        Map<String, String> props = _delegate.getProperties(path);
        props.remove(PASSWORD_PROPNAME);
        return props;
    }
    
    
    /**
     * Filters the directory listing provided by the delegate storage.
     * There should be a hierarchical traverse access to each of the
     * returned paths.
     */
    protected Iterator<String> postFilterDirListing(Iterator<String> iterator) throws StorageException {
        List<String> list = new ArrayList<String>();
        
        while(iterator.hasNext()) {
            String path = iterator.next();
            
            try {
                checkHierarchicalTraverseAccess(path);
                list.add(path);
            }
            catch(InsufficientStorageAccessPermissionsException exception) {}
        }
        
        return list.iterator();
    }
    
    
    /**
     * Filters the directory listign provided by the delegate storage.
     * There should be a hierarchical traverse access to each of the
     * returned paths.
     */
    protected <T> Map<String, T> postFilterPathMap(Map<String, T> inMap) throws StorageException {
        HashMap<String, T> outMap = new HashMap<String, T>();
        
        Iterator<String> iterator = inMap.keySet().iterator();
        while(iterator.hasNext()) {
            String path = iterator.next();
            
            try {
                checkHierarchicalTraverseAccess(path);
                outMap.put(path, inMap.get(path));
            }
            catch(InsufficientStorageAccessPermissionsException exception) {}
        }
        
        return outMap;
    }
    

    /**
     * Checks if the action on the give path is possible for the current user.
     * @param path The path.
     * @param action The action.
     * @throws StorageException
     */    
    protected void checkFlatAccess(String path, ACLResourceAction action) throws StorageException {
        FilePathACLResource resource = new FilePathACLResource(path);
        if (!hasRights(resource, action))
            throw new InsufficientStorageAccessPermissionsException(_owner.getId(), path, "Not enough permissions for the user to perform action: "  + action.getClass().toString() + ".");
    }
    

    /**
     * Checks if access is possible through the parent directories on the
     * given path.
     * @param path Absolute path from the root of the repository.
     * @throws InsufficientStorageAccessPermissionsException
     */    
    protected void checkHierarchicalTraverseAccess(String path) throws StorageException {
    	FilePathACLResource currentParentResource = new FilePathACLResource(path);
        while((currentParentResource = (FilePathACLResource) currentParentResource.getParentResource()) != null)
            checkFlatTraverseAccess(currentParentResource);
    }
    
    
    /**
     * Checks if it is possible to traverse only this path with the assumption
     * that traversal through its parent is possible.
     * 
     * Traversal is possible if and only if:
     * * path is a directory
     * * the DIRECTORY_TRAVERSE_ACTION is possible for the current user on this path.
     */
    protected void checkFlatTraverseAccess(FilePathACLResource resource) throws StorageException {
        if (_delegate.fileExists(resource.getIdentifier())) {
            if (!_delegate.isDirectory(resource.getIdentifier()))
                throw new InsufficientStorageAccessPermissionsException(_owner.getId(), resource.getIdentifier(), "It is not a directory");
        
            if (!hasRights(resource, GwACLResourceActions.BROWSE_ACTION))
                throw new InsufficientStorageAccessPermissionsException(_owner.getId(), resource.getIdentifier(), "No permission to traverse it");
        }
    }

    /**
     * Checks if the user has certain rights for a resource and action on this storage.
     * Note that we view rights only for the single file. 
     */
    private boolean hasRights( final FilePathACLResource resource, final ACLResourceAction action ) throws StorageException {
        StorageACLAuthorizer authorizer = new StorageACLAuthorizer(_delegate);
        return authorizer.hasAccess(resource, action, _owner);
    }
}
