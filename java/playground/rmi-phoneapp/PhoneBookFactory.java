package phoneapp;

import java.rmi.*;

/**
 * This is a remote interface which is implemented by a server that will
 * produce objects of the {@link PhoneBook} type at the request of clients.
 * <br><br>
 * The process that takes place is this:
 * <ol>
 * <li> The client contacts a PhoneBookFactory server (an implementation
 * of the the PhoneBookFactory interface) and requests a new {@link PhoneBook}
 * object to be created.
 * <li> The PhoneBookFactory server creates the requested object and returns
 * a reference to it to the client. Actually, what the client will get is a
 * reference to a PhoneBookFactory_Stub.
 * <li> The client can use RMI to invoke methods of the newly obtained
 * remote object (the PhoneBookFactory_Stub instance).
 * </ol>
 * <br>
 * This way, the only object that needs to be registered is an instance of
 * the phone book factory.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0, 19.08.2001
 * @see "RMI documentation"
 */
public interface PhoneBookFactory extends Remote
{
	/**
	 * Creates a new instance of a {@link PhoneBook} and returns it.
	 * This is the minimal required behaviour.
	 * <br><br>
	 * @param owner the owner of the newly created phone book.(?!)
	 * @return a reference to the newly created {@link PhoneBook}
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public PhoneBook getPhoneBook(String owner) throws RemoteException;
}
