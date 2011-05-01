package gw.storage;

import gw.storage.javahl.SVNStorage;
import gw.storage.javasvn.SVNRAStorage;
import gw.storage.javasvn.virtualwc.VirtualWCStorage;

/**
 * Makes Storage objects.
 * 
 * TODO: Turn this into an interface and make a "SessionSVNStorageFactory" that
 *       implements it.
 */
public class SessionStorageFactory {
    /**
     * Returns a storage (specifically, an SVNStorage) given a svn username, password and 
     * svn repository URI.
     * 
     * @param username The svn username.
     * @param password The svn password.
     * @param repository The repository URI.
     */
    public static Storage getSVNStorage(String username, String password, String repository)
    		throws StorageException
    {
    	//return new VirtualWCStorage(repository, username, password);
        //return new SVNStorage(repository, username, password);
    	return new SVNRAStorage(repository, username, password);
    }
}
