package gw.users.acl;

/**
 * A set of permissions.
 */
public class GwACLPermissions {
    public static final String ACL_PREFIX = "acl:";
    
    public static final ACLPermission READ_PERMISSION           = new NamedACLPermission(ACL_PREFIX + "read");
    public static final ACLPermission WRITE_PERMISSION          = new NamedACLPermission(ACL_PREFIX + "write");
    public static final ACLPermission DELETE_PERMISSION         = new NamedACLPermission(ACL_PREFIX + "delete");
	public static final ACLPermission BROWSE_PERMISSION         = new NamedACLPermission(ACL_PREFIX + "browse");
    public static final ACLPermission PASSWORD_PERMISSION       = new NamedACLPermission(ACL_PREFIX + "password");
    public static final ACLPermission ACL_WRITE_PERMISSION      = new NamedACLPermission(ACL_PREFIX + "writeACL");

    public static final ACLPermission[] ALL_PERMISSIONS = new ACLPermission[]
        { READ_PERMISSION
        , WRITE_PERMISSION
        , DELETE_PERMISSION
        , BROWSE_PERMISSION
        , PASSWORD_PERMISSION
        , ACL_WRITE_PERMISSION
        };
    
    /**
     * Gets a permission by its name.
     * @param name The name of the permission (e.g. acl:read).
     * @return The permission.
     */
    public static ACLPermission getPermissionByName(final String name) {
        for(int i=0; i < ALL_PERMISSIONS.length; i++)
            if (ALL_PERMISSIONS[i].getIdentifier().equals(name))
                return ALL_PERMISSIONS[i];

        throw new RuntimeException("ACLPermission " + name + " does not exist.");
    }
}
