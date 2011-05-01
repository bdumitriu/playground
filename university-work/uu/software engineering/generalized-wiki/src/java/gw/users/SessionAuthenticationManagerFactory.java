package gw.users;

import gw.GwSessionContext;
import gw.storage.*;

/**
 * The session authentication manager factory constructs session authentication managers.
 */

public class SessionAuthenticationManagerFactory implements AuthenticationManagerFactory {
    private GwSessionContext _sessionContext;
    
    /**
     * Create a SessionAuthenticationManagerFactory
     * @param sessionContext the GwSessionContext that has resources available for this factory.
     */
    public SessionAuthenticationManagerFactory(GwSessionContext sessionContext) {
        if(sessionContext == null)
            throw new IllegalArgumentException("SAMFactory: session context cannot be null");
        
        _sessionContext = sessionContext;
    }
    /**
     * Constructs a session authentication manager for the current session.
     * @param unsecuredStorage the storage to use.
     * @return the authentication manager.
     */
    public AuthenticationManager getAuthenticationManager(Storage unsecuredStorage) {
        UserDataAdapter uda = UserDataAdapterFactory.getUserDataAdapter(unsecuredStorage);
        SessionAuthenticationManager manager = new SessionAuthenticationManager(_sessionContext, uda);
        
        return manager;
    }
}
