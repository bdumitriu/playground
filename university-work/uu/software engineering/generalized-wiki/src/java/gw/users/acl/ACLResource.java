package gw.users.acl;

/**
 * An ACLResource represents an identity that can have ACLs applied to it. It accesses
 * a unique String identifier and a parent. The ACL system uses this parent
 * method to walk upwards on a hierarchy.
 */
public interface ACLResource {
    /**
     * Gets an identifier to the resource.
     * @return A String representing the identity of this ACLResource
     */
    public String getIdentifier();    
    
    /**
     * Gets the parent resource of this resource.
     * @return An ACLResource which is a parent of this ACLResource. 
     */
    public ACLResource getParentResource();
}

