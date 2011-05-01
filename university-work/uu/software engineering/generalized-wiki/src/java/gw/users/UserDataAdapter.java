package gw.users;

import java.util.List;

/**
 * A interface to store or load data of a given user to a location.
 */
public interface UserDataAdapter {

	/**
	 * Creates a entry on the location for the given user.
	 * @param user the user to be created on the location.
	 */
	public void create(final User user, final String password );

    /**
     * Set the password of the given user
     * @param user the user whoms password must be set
     * @param password the new password of the given user (cleartext)
     */
    public void setPassword( final User user, final String password);

    /**
     * Genenerate the hash generated for the given user.
     * @param user user to get the password from
     * @return the hash of the user.
     */
    public String getPassword( final User user );

	/**
	 * Deletes the entry on the location for the given user.
	 * @param user the user to be deleted on the location.
	 */
	public void delete(final User user) throws NoSuchUserException;
	
	/**
	 * Load the properties of the given user from the location. The userid must be a existing userid
	 * otherwise a NoSuchUserException will be thrown
	 * @param user the user to be loaded.
	 */
	public void load(final User user) throws NoSuchUserException;

	/**
	 * Store the properties of the given user to the location. If the user does not exist, it
	 * will be created.
	 * @param user the user to store.
	 */
	public void store(final User user);

	/** 
	 * Get a list of all users that are stored on the location.
	 * @return a list with all users stored on the location.
	 */
	public List getAllUsers();
	
	/**
	 * True if the given user with the userid exists.
	 * @param user user with a userid.
	 * @return true if the user with the userid exists, false otherwise
	 */
	public boolean exists( final User user );
}
