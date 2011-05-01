package gw.users.acl;

/**
 * Represents a permission identified by a name.
 */
class NamedACLPermission implements ACLPermission {
    private String _name;
    
    public NamedACLPermission(String name) {
        _name = name;
    }
    
    public String getIdentifier() {
        return _name;
    }
}