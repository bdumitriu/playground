package gw.users;


import java.util.List;


/**
 * The user manager is an entry-point for managing users.
 */
public interface UserManager {
	/**
	 * Creates the user.
	 * @param id The user id (= user name).
	 * @param email The user his email address.
	 * @param password The password of the user.
	 * @return The created user account.
	 */
	public User createUser(
		final String id,
		final String email,
		final String password);


	/**
	 * @return a list of all users.
	 */
	public List getUsers();

	/**
	 * Checks if the given user already exist.
	 */
	public boolean existUser(final String id);
	/**
	 * Returns the user with the given id or throws an
	 * exception.
	 */

	public User getUser(final String id);

	/**
	 * Sync the contents of the user object with the datastore facility.
	 */
	public void syncUser(final User user);

    /**
     * Get the digest of the users password.
     * @param user the user whoms passworddigest must be returned
     * @return the digest of the users password.
     */
    public String getPassword(final User user);

    /**
     * Set the password of the given user to the given string (cleartext).
     * @param user the user where the password from is changed
     * @param clearPassword the password in cleartext.
     */
    public void setPassword(final User user, final String clearPassword );
}
