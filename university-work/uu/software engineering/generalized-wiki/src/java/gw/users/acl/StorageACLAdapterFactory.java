package gw.users.acl;

import gw.storage.*;

/**
 * Constructs Storage ACL Adapters.
 */
public class StorageACLAdapterFactory implements ACLAdapterFactory {
	private Storage _storage;
	
	public StorageACLAdapterFactory(Storage storage) {
		_storage = storage;
	}
	
    /**
     * Returns the ACLAdapter belonging to a certain file.
     */
    public ACLAdapter getACLDefinition(ACLResource resource) {
    	return new StorageACLAdapter(resource.getIdentifier(), _storage);
    }
}
