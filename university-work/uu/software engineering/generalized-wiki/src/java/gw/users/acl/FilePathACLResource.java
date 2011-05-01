package gw.users.acl;

/**
 * A file path resource receives a certain path in the unix style
 * path notation. The path should be absolute with respect to the
 * root of the repository. It does not have to exist.
 */
public class FilePathACLResource implements ACLResource {
    private String _fullpath;    
    
    /**
     * Initializes the ACLResource.
     */
    public FilePathACLResource(String fullpath) {
        if (fullpath == null)
            throw new IllegalArgumentException("fullpath == null");
        
        if (fullpath.endsWith("/") && fullpath.length() > 1)
            _fullpath = fullpath.substring(0, fullpath.length()-1);
        else
            _fullpath = fullpath;
    }

	/**
     * Gets the indentifier of the resource, which is the full path.
	 * @see gw.users.acl.ACLResource#getIdentifier()
	 */
	public String getIdentifier() {
		return _fullpath;
	}

	/**
     * Gets the parent path by stripping of the last '/filename';
     * Null is returned if there is no parent. Slash "/" is returned
     * for the root. The root has no parent.
	 * @see gw.users.acl.ACLResource#getParentResource()
	 */
	public ACLResource getParentResource() {
        if (_fullpath.equals("/"))
            return null;
        
        int splitIndex = _fullpath.lastIndexOf("/");
        if (splitIndex < 0)
            return null;
        else if (splitIndex == 0)
            return new FilePathACLResource("/");
        else {
            String parent = _fullpath.substring(0, splitIndex);
            return new FilePathACLResource(parent);
        }        
	}
    
    
    /**
     * Returns the name of the resource, excluding the parents.
     * @return The name.
     */
    public String getName() {
        int splitIndex = _fullpath.lastIndexOf("/");
        if (splitIndex <= 0)
            return _fullpath;

        return _fullpath.substring(splitIndex + 1);
    }
}
