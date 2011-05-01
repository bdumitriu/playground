package gw.users;


/**
 * The user data adapter factory constructs user managers.
 */
public class DataAdapterUserManagerFactory implements UserManagerFactory {
    private UserDataAdapter _uda;
    
    public DataAdapterUserManagerFactory(UserDataAdapter uda) {
        _uda = uda;
    }
    
    /**
     * Constructs an authentication manager for the current session.
     * @return the authentication manager.
     */
    public UserManager getUserManager() {
        return new DataAdapterUserManager(_uda);
    }
}
