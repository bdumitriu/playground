package gw.users.acl;

import java.util.List;
import gw.storage.*;

/**
 * An ACL Adapter is an interface to the contents of an
 * Access Control List defined on some file or directory.
 * 
 * The definition just tells what information is stored in the
 * ACL. Reasoning with it is build on top of this.
 */
public interface ACLAdapter {
    /**
     * Checks if the ACL contains an non-empty entry for a
     * certain permission.
     * @param permission The permission.
     * @return True if the non-empty entry exists, false otherwise.
     * @throws StorageException
     */
    public boolean permissionEntryExists(ACLPermission permission) throws StorageException;
    
    /**
     * Gets the users associated with the permission.
     * @param permission The permission.
     * @return A list of usernames that are defined at the permission entry.
     * @throws StorageException
     */
    public List getUsersForPermission(ACLPermission permission) throws StorageException;
	
    /**
	* Writes the right for a user to the ACL file.
	* @param p This is the permission to be written.
	* @param aliasId This is the user or group.
    * @throws StorageException
	*/
	public void addACLRight(ACLPermission p, String aliasId) throws StorageException;

	/**
	* Removes the right from the ACL file.
	* @param p This is the permission to be removed.
	* @param aliasId This is the user or group.
    * @throws StorageException
	*/
	public void removeACLRight(ACLPermission p, String aliasId) throws StorageException;
    
    /**
     * Sets the users or groups for this permission.
     * @param permission The permission.
     * @param aliasses The aliasses.
     * @throws StorageException
     */
    public void setUsersForPermission(ACLPermission permission, List aliasses) throws StorageException;
    
    /**
     * Sets the line for this acl permission.
     * @param permission The permission.
     * @param value The string with comma separated usernames.
     * @throws StorageException
     */
    public void setPermissionLine(ACLPermission permission, String value) throws StorageException;
    
    /**
     * Gets the line for this acl permission.
     * @param permission The permission.
     * @throws StorageException
     */
    public String getPermissionLine(ACLPermission permission) throws StorageException;
}
