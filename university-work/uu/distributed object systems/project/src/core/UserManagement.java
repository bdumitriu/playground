package core;

import core.exceptions.AuthenticationFailedException;
import core.exceptions.DuplicateLoginNameException;
import core.exceptions.InvalidLoginNameException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

/**
 * This interface contains all the methods which need to be implemented for providing user management facilities.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 8, 2005
 */
public interface UserManagement extends Remote
{

	/**
	 * Authenticates a user by login name and password. If successful, a valid {@link User} object will be returned.
	 * Otherwise, null is returned.
	 *
	 * @param userName the login name of the user that wants to get authenticated.
	 * @param password the password of the user.
	 * @return the User object that represents the user, or null if the supplied credentials are incorrect.
	 */
	User authenticate(String userName, String password) throws RemoteException;

	/**
	 * Logs the user off the system. This means that the user will no longer be able to use his refernce to the
	 * {@link User} remote object, since this object will be deactivated.
	 *
	 * @param authToken a token by which the system can check if the caller is logged in and also identify him.
	 * @throws core.exceptions.AuthenticationFailedException if the supplied <code>authToken</code> is invalid.
	 */
	void logOff(String authToken)
		throws RemoteException, AuthenticationFailedException;

	/**
	 * Returns a list of all the users of the system. Only the login name and the user name of the users is given.
	 *
	 * @param authToken a token by which the system can check if the caller is logged in and also identify him.
	 * @return a list of all the users of the system.
	 * @throws core.exceptions.AuthenticationFailedException if the supplied <code>authToken</code> is invalid.
	 */
	List<UserData> getAllUsers(String authToken)
		throws RemoteException, AuthenticationFailedException;

	/**
	 * Creates an account for a new user.
	 *
	 * @param details the login name and the real name of the user.
	 * @param password this will be the password for the user.
	 * @param granularity the granularity for the user.
	 * @param phoneNr the phone number of the user.
	 * @param title the title of the user (Mr, Mrs, Miss, etc.).
	 * @throws DuplicateLoginNameException if there is already another user which uses the same login name.
	 */
	void createAccount(UserData details, String password, Granularity granularity, String phoneNr, String title)
		throws RemoteException, DuplicateLoginNameException;

	/**
	 * Deletes the account of a user (after previously logging him off).
	 *
	 * @param authToken a token by which the system can check if the caller is logged in and also identify him.
	 * @throws core.exceptions.InvalidLoginNameException if the login name of the user object associated with the
	 *	<code>authToken</code> does not identify a valid user of the system.
	 * @throws core.exceptions.AuthenticationFailedException if the supplied <code>authToken</code> is invalid.
	 */
	void deleteAccount(String authToken)
		throws RemoteException, InvalidLoginNameException, AuthenticationFailedException;
}
