package core;

import core.exceptions.InvalidLoginNameException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.io.Serializable;

/**
 * This interface defines the methods that have to be supported by any class that will be used to represent the user
 * of this system.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 9, 2005
 */
public interface User extends Remote
{
	/**
	 * Returns the authentication token to be used for this session. This will be needed when calling various
	 * methods of the {@link GroupwareManagement} class.
	 *
	 * @return the authentication token to be used for this session.
	 */
	public String getAuthToken() throws RemoteException;

	/**
	 * Returns the login name for this user.
	 *
	 * @return the login name for this user.
	 */
	public String getLoginName() throws RemoteException;

	/**
	 * Sets the full name of this user to <code>name</code>.
	 *
	 * @param name the full name of this user.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setName(String name) throws RemoteException, InvalidLoginNameException;

	/**
	 * Returns the full name of this user.
	 *
	 * @return the full name of this user.
	 */
	public String getName() throws RemoteException;

	/**
	 * Sets the granularity for this user to <code>granularity</code>.
	 *
	 * @param granularity the granularity for this user.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setGranularity(Granularity granularity) throws RemoteException, InvalidLoginNameException;

	/**
	 * Returns the granularity for this user.
	 *
	 * @return the granularity for this user.
	 */
	public Granularity getGranularity() throws RemoteException;

	/**
	 * Sets the title (Mr, Mrs, Miss, etc.) of this user to <code>title</code>.
	 *
	 * @param title the title of this user.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setTitle(String title) throws RemoteException, InvalidLoginNameException;

	/**
	 * Returns the title of this user.
	 *
	 * @return the title of this user.
	 */
	public String getTitle() throws RemoteException;

	/**
	 * Sets the phone number of this user to <code>phoneNumber</code>.
	 *
	 * @param phoneNumber the phone number of this user.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setPhoneNumber(String phoneNumber) throws RemoteException, InvalidLoginNameException;

	/**
	 * Returns the phnoe number of this user.
	 *
	 * @return the phnoe number of this user.
	 */
	public String getPhoneNumber() throws RemoteException;

	/**
	 * Returns an instance of {@link Calendar} which represents the calendar of this user.
	 *
	 * @return an instance of {@link Calendar}.
	 */
	public Calendar getCalendar() throws RemoteException;
}
