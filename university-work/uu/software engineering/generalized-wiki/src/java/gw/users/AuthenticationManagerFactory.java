package gw.users;

import gw.storage.*;

/**
 * The authentication manager factory constructs authentication managers.
 *
 * Authentication managers are not singletons. For example, a session authentication
 * manager has to be private for each session. In order to get such an authentication
 * manager, you'll have to construct the manager using this factory.
 * 
 * TODO: write unit tests.
 */
public interface AuthenticationManagerFactory {
	/**
     * Constructs an authentication manager for the current session.
     * @param unsecuredStorage the storage to use.
     * @return the authentication manager.
     */
    public abstract AuthenticationManager getAuthenticationManager(Storage unsecuredStorage);
}
