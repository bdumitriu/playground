package phoneapp;

import java.rmi.*;
import java.util.Vector;

/**
 * This is the remote interface which is supposed to be implemented by a
 * server that will provide phone book support to various clients.
 * <br><br>
 * What the server must offer are typical phone book actions:
 * <ul>
 * <li> add an entry;
 * <li> delete an entry;
 * <li> look a number up;
 * <li> list all entries;
 * <li> save the phone book in a file;
 * <li> restore the phone book from a file.
 * </ul>
 * Any server that implements this interface will have to provide this
 * minimum functionality.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0, 19.08.2001
 * @see "RMI documentation"
 */
public interface PhoneBook extends Remote
{
	/**
	 * Adds an entry to the phone book.
	 * <br><br>
	 * @param entry the entry to be added.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public void addEntry(Entry entry) throws RemoteException;
	
	/**
	 * Looks up the number(s) corresponding to the person specified by
	 * name. The result you get should be used like this:
	 * <pre>
	 *	Vector result = remoteClientObject.lookupNumber("...");
	 *	String numbers[] = (String[]) result.toArray();
	 * </pre>
	 * thus obtaining an array of strings representing the phone number(s)
	 * of the person with the name supplied as an argument.
	 * <br><br>
	 * @param name the name of the person whose phone number(s) is(are)
	 *	required.
	 * @return a Vector of strings representing the phone number(s).
	 *	Usually, this Vector will only have one element.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public Vector lookupNumber(String name) throws RemoteException;
	
	/**
	 * Deletes an entry from the phone book.
	 * <br><br>
	 * @param entry the entry to be deleted.
	 * @see #deleteEntry(String name, String number)
	 * @return true if entry has been deleted successfuly and false if
	 *	the entry hasn't been found.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public boolean deleteEntry(Entry entry) throws RemoteException;
	
	/**
	 * Deletes an entry from the phone book. The entry is denoted by the
	 * name and the number of the person that has to be deleted.
	 * <br><br>
	 * @param name the name part of the entry.
	 * @param number the number part of the entry.
	 * @see #deleteEntry(Entry entry)
	 * @return true if entry has been deleted successfuly and false if
	 *	the entry hasn't been found.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public boolean deleteEntry(String name, String number)
		throws RemoteException;
	
	/**
	 * Returns a Vector with all the entries in the phone book.
	 * You could use the result like this:
	 * <pre>
	 *	Vector result = remoteClientObject.list();
	 *	Entry entries[] = (Entry[]) result.toArray();
	 * </pre>
	 * thus obtaining an array of entries representing the phone book
	 * contents.
	 * <br><br>
	 * @return a Vector with all the entries in the phone book.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public Vector list() throws RemoteException;
	
	/**
	 * Saves the current contents of the phone book in the specified file.
	 * <br><br>
	 * @param filename the name of the file to save the phone book to.
	 * @see #restore(String filename)
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public void save(String filename) throws RemoteException;
	
	/**
	 * Restores the phone book from the specified file.
	 * <br><br>
	 * @param filename the name of the file to restore the phone book
	 *	from.
	 * @see #save(String filename)
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 * @throws java.lang.ClassNotFoundException if class of a serialized
	 	object cannot be found during deserialization from the
	 	specified file.
	 */
	public void restore(String filename) throws RemoteException,
		ClassNotFoundException;
}
