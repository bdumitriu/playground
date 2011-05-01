package gw.users.acl;


/**
 * Interface for ACL Adapter Factories.
 */

public interface ACLAdapterFactory {
    /**
     * Returns the ACLAdapter belonging to a certain resource.
     */
    public abstract ACLAdapter getACLDefinition(ACLResource resource);
}
