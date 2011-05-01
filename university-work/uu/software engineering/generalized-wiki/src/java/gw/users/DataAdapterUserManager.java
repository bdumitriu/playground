package gw.users;


import java.util.*;


/**
 * The user manager is an entrypoint for user management. It requires
 * a user data adapter to do its work. This layer on top of the UDA
 * ensures indepenence of storage facility.
 * 
 * The UserManager is an entry-point for management of users.
 */
public class DataAdapterUserManager implements UserManager {
    private UserDataAdapter _uda;
    
    
    /**
     * Constructs the default user manager. It uses the user
     * data adapter to store and retrieve user information.
     * @param uda The User Data Adapter.
     */
    DataAdapterUserManager(UserDataAdapter uda) {
        if (uda == null)
            throw new IllegalArgumentException("uda is null");
        
        _uda = uda;
    }
    
    
    /**
     * Creates the user.
     * @param id The user id (= user name).
     * @param email The user his email address.
     * @param password The password of the user.
     * @return The created user account.
     */
    public User createUser(final String id, final String email, final String password) {
        if (existUser(id))
            throw new IllegalArgumentException("User with id: " + id + " already exists.");
        
        if( email == null )
            throw new NullPointerException( "gw.users.UserManager.email == null" );

        User user = new User(id, email);

        _uda.create(user, password);
        return user;
    }
    
    
    /**
     * @return a list of all users.
     */
    public List getUsers() {
        return _uda.getAllUsers();
    }
    
    
    /**
     * Checks if the given user already exist.
     */
    public boolean existUser(final String id) {
         
        return _uda.exists( new User(id) );
    }
    
    
    /**
     * Returns the user with the given id or throws an
     * exception.
     */
    public User getUser(final String id) {
        User user = new User(id);
        _uda.load(user);
        return user;
    }
   
    /**
     * Sync the contents of the user object with the datastore facility.
     */
    public void syncUser(User user) {
        _uda.store(user);
    }

    /**
     * Gets the (encrypted) password of the user.
     */
    public String getPassword( User user ) {

        return _uda.getPassword( user);
    }

    /**
     * Sets the password of the user.
     */
    public void setPassword( User user, String clearPassword ) {

        _uda.setPassword( user, clearPassword );
    }
}
