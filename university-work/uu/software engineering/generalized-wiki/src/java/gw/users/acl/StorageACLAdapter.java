package gw.users.acl;

import java.util.*;
import gw.storage.*;

/**
 * This class handles the rights of the ACL files. By using this class rights can be
 * added, removed and checked. This class uses the SVN storage interface and can only
 * be used on files stored in the SVN storage.
 */
public class StorageACLAdapter implements ACLAdapter {
    private String   _pathName;
    private Storage  _storage;
    
    /**
     * Initializes the StorageACLAdapter with the path of the file the ACL belongs to
     * and an interface to the storage.
     * @param pathName The pathname of the file the ACL belongs to.
     * @param storage  The storage.
     */
    public StorageACLAdapter(String pathName, Storage storage) {
        _pathName = pathName;
        _storage  = storage;
    }    
    
    /**
     * Returns the list of users for the permission, or null if there is no
     * such permission line.
     * @see gw.users.acl.ACLAdapter#getUsersForPermission(gw.users.acl.ACLPermission)
     */
    public List getUsersForPermission(ACLPermission permission) throws StorageException {
        String permLine = getPermissionLine(permission);
        
        if (permLine == null || permLine.trim().equals(""))
            return null;
        
        return getUserIdsFromPermissionLine(permLine);
    }

    /**
     * Returns true if and only if the corresponding property is defined on the file and
     * not empty (not only whitespace).
     * @see gw.users.acl.ACLAdapter#permissionEntryExists(gw.users.acl.ACLPermission)
     */
    public boolean permissionEntryExists(ACLPermission permission) throws StorageException {
        String permLine = getPermissionLine(permission);
        return permLine != null && !permLine.trim().equals("");
    }

    /**
     * Removes the permission for a specific user or group.
     * @see gw.users.acl.ACLAdapter#removeACLRight(gw.users.acl.ACLPermission, java.lang.String)
     */
    public void removeACLRight(ACLPermission permission, String aliasses) throws StorageException {
        List users = getUsersForPermission(permission);
        if (users != null) {
            users.remove(aliasses);
            setUsersForPermission(permission, users);
        }
    }

    /**
     * Adds the permission for the specific user or group.
     * @see gw.users.acl.ACLAdapter#addACLRight(gw.users.acl.ACLPermission, java.lang.String)
     */
    public void addACLRight(ACLPermission permission, String aliasId) throws StorageException {
        List users = getUsersForPermission(permission);
        if (users == null)
            users = new ArrayList();
        
        users.add(aliasId);
        setUsersForPermission(permission, users);
    }
    
    /**
     * Sets the users for this permission.
     * 
     * The users are concatenated user ','. We accomplish this by iterating
     * all users and emitting ", userid". The resulting string is obtained
     * by stripping of the first ",". This first "," is only there if there
     * are actually users. 
     * 
     * @param permission The permission.
     * @param aliasses The aliasses.
     * @throws StorageException
     */
    public void setUsersForPermission(ACLPermission permission, List aliasses) throws StorageException {
        String line = "";
        
        Iterator iterator = aliasses.iterator();
        while(iterator.hasNext()) {
            String user = (String) iterator.next();
            line += ", " + user;
        }
        
        if (aliasses.size() > 0)
            line = line.substring(1);

        setPermissionLine(permission, line);
    }
      
    /**
     * Gets the permission line stored in the acl for the given permission. Returns
     * null if the permission does not exist or is not defined.
     * @see gw.users.acl.ACLAdapter#getPermissionLine(gw.users.acl.ACLPermission)
     */
    public String getPermissionLine(ACLPermission permission) throws StorageException {
        Map props = _storage.getProperties(_pathName);
        return (String) props.get(permission.getIdentifier());
    }
    
    /**
     * Sets the line for this acl permission.
     * @param permission The permission.
     * @param value The string with comma separated usernames.
     * @throws StorageException
     */
    public void setPermissionLine(ACLPermission permission, String value) throws StorageException {
        
        _storage.setProperty(_pathName, permission.getIdentifier(), value.trim(), false);
  	}
    
    /**
     * Returns the user ids on the comma separated line.
     * @param line The line with the user ids.
     * @return The user ids.
     */
    private List getUserIdsFromPermissionLine(String line) {
        ArrayList list = new ArrayList();
        StringTokenizer tokenizer = new StringTokenizer(line, ",");        
        
        while(tokenizer.hasMoreTokens()) {
            String userId = tokenizer.nextToken().trim();
            
            if (!userId.equals(""))
                list.add(userId);
        }
            
        return list;            
    }
}
