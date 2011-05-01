package gw.users.acl;

import java.util.*;
import gw.users.*;
import gw.storage.*;

/**
 * Provides facilities to check if a certain user has access to a certain
 * file resource to apply a certain action. For a user, we generate all the
 * possible usernames (groupnames) and check which permissions are defined
 * for them.
 * Since this is a Storage Authorizer, we accept only FilePath resources.
 */
public class StorageACLAuthorizer {
    public static final String ADMINISTRATOR_ID = "Administrator";
    
    private Storage                 _storage;
    private UserAliasDictionary     _aliasDictionary;    
    
    public StorageACLAuthorizer(Storage storage) {
        _storage         = storage;
        _aliasDictionary = new StorageUserAliasDictionary(storage);
    }
    
    /**
     * Checks if the given user has access to the resource to apply the given
     * action.
     * 
     * If there is no ACL defined for a given permission on the current
     * resource, the parent ACL which defines it will be used. If there
     * is no such ACL, then all access is granted.
     * 
     * The user or group with id "Administrator" is always granted access.
     * 
     * @param resource The resource.
     * @param action The action.
     * @param user The user.
     * @return True if and only if the given user is element of the allowed
     *         users defined for the resource and action pair.
     */
    public boolean hasAccess(final FilePathACLResource resource, final ACLResourceAction action, final User user) throws StorageException {
        List usernames = _aliasDictionary.getAliases(user.getId());
        
        // An administrator always has access.
        if (usernames.contains(ADMINISTRATOR_ID))
            return true;
        
        Iterator permissions = action.getRequiredPermissions().iterator();
        while(permissions.hasNext()) {
            ACLPermission permission = (ACLPermission) permissions.next();
            ACLAdapter acl = getEffectiveACLDefinition(resource, permission);
            
            // No acl defined for the permission:
            // silently accept access for this permision.
            if (acl == null)
                continue;
            
            if (!hasPermission(acl, permission, usernames))
                return false;
        }
        
        return true;
    }
    
    
    /**
     * Checks if the ACLAdapter permission line references the user.
     * @param acl The ACL.
     * @param permission The permission.
     * @param usernames The set of usernames.
     * @return True if there is permission, false otherwise.
     */
    protected boolean hasPermission(final ACLAdapter acl, final ACLPermission permission, final List usernames) throws StorageException {
        List allowedUsers = acl.getUsersForPermission(permission);
        
        if (allowedUsers != null)
        {
        	Iterator allowedUsersIterator = allowedUsers.iterator();
        
	        while(allowedUsersIterator.hasNext()) {
	            String allowedUsername = (String) allowedUsersIterator.next();
	            
	            Iterator availableUsersIterator = usernames.iterator();
	            while(availableUsersIterator.hasNext()) {
	                String availableUsername = (String) availableUsersIterator.next();
	                
	                if (allowedUsername.equals(availableUsername))
	                    return true;
	            }
	        }
        }

        return false;
    }
    
    
    /**
     * Gets the ACLAdapter defined for the resource that has the given permission.
     * @param resource The resource.
     * @param permission The permission.
     * @return An ACLAdapter.
     */
    protected ACLAdapter getEffectiveACLDefinition(final ACLResource resource, final ACLPermission permission) throws StorageException {
        if (resource == null)
            throw new IllegalArgumentException("ACLResource is null.");
                    
        ACLResource current = resource;
        while (current != null) {
            if (_storage.fileExists(current.getIdentifier())) {
                ACLAdapter definition = (new StorageACLAdapterFactory(_storage)).getACLDefinition(current);
            
                if (definition.permissionEntryExists(permission))
                    return definition;
            }
            
            current = current.getParentResource();
            if (current == null)
                break;
        }
        
        return null; // no ACL defined on hierarchy for the given permission.
    }
}
