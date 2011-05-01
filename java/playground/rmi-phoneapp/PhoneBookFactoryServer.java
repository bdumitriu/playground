package phoneapp;

import java.rmi.*;
import java.rmi.server.*;

/**
 * This class represents a possible implementation of the {@link
 * PhoneBookFactory}. Instances of this class will be used as creators
 * (on request) of {@link PhoneBookServer} instances.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0, 21.08.2001
 * @see "RMI documentation"
 */
public class PhoneBookFactoryServer extends UnicastRemoteObject
	implements PhoneBookFactory
{
	/**
	 * Creates a new PhoneBookFactoryServer which can be added as is to a
	 * remote object registry.
	 * <br><br>
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public PhoneBookFactoryServer() throws RemoteException
	{
		super();
	}

	/**
	 * This method does exactly the minimum it has to do (what the
	 * {@link PhoneBookFactory} specifies). That is, it creates a new
	 * {@link PhoneBookServer} object and returns it.
	 * <br><br>
	 * @param owner the owner of the newly created phone book.(?!)
	 * @return a reference to the newly created {@link PhoneBookServer}
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public PhoneBook getPhoneBook(String owner) throws RemoteException
	{
		PhoneBookServer phoneBook = new PhoneBookServer(owner);
		return phoneBook;
	}
}
