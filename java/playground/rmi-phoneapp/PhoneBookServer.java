package phoneapp;

import java.rmi.*;
import java.rmi.server.*;
import java.io.*;
import java.util.Vector;

/**
 * This class represents a possible implementation of the {@link PhoneBook}
 * interface. Objects that instantiate this class allow clients to invoke
 * their methods in order to accomplish specific phone book actions.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0, 21.08.2001
 * @see "RMI documentation"
 */

public class PhoneBookServer extends UnicastRemoteObject implements PhoneBook
{
	/*
	 * Used for internal storage of Entry objects.
	 */
	private Vector entries;

	/**
	 * Creates a new PhoneBookServer object which can be added as is to a
	 * remote object registry.
	 * <br><br>
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public PhoneBookServer(String owner) throws RemoteException
	{
		super();
		entries = new Vector();
	}

	/**
	 * Adds an entry to the phone book.
	 * <br><br>
	 * @param entry the entry to be added.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public void addEntry(Entry entry) throws RemoteException
	{
		entries.addElement(entry);
	}

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
	public Vector lookupNumber(String name) throws RemoteException
	{
		Vector numbers = new Vector();
		
		for (int i = 0; i < entries.size(); i++)
		{
			Entry entry = (Entry) entries.elementAt(i);
			
			if (entry.getName().equals(name))
				numbers.addElement(entry.getNumber());
		}
		
		return numbers;
	}

	/**
	 * Deletes an entry from the phone book. The comparison is made using
	 * the {@link Entry#equals(Object obj) equals} method implemented in
	 * the {@link Entry} class. That means that it is enough for the entry
	 * supplied as a parameter to have the same name and number with the
	 * one to be deleted for them to be considered equal; they do NOT
	 * (necesarry) have to point to the same object.
	 * <br><br>
	 * Beware that if there is more than one occurence of the same entry
	 * only the first will be deleted, so you should call deleteEntry
	 * in a loop until you get a "false" return if you want to delete
	 * all occurences.
	 * <br><br>
	 * @param entry the entry to be deleted.
	 * @see #deleteEntry(String name, String number)
	 * @return true if entry has been deleted successfuly and false if
	 *	the entry hasn't been found.
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public boolean deleteEntry(Entry entry) throws RemoteException
	{
		return entries.removeElement(entry);
	}

	/**
	 * Deletes an entry from the phone book. The entry is denoted by the
	 * name and the number of the person that has to be deleted. The same
	 * warning applies here as in the case of the {@link
	 * #deleteEntry(Entry entry)} method.
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
		throws RemoteException
	{
		return deleteEntry(new Entry(name, number));
	}

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
	public Vector list() throws RemoteException
	{
		return entries;
	}

	/**
	 * Saves the current contents of the phone book in the specified file.
	 * Serialization is used for the process.
	 * <br><br>
	 * @param filename the name of the file to save the phone book to.
	 * @see #restore(String filename)
	 * @throws java.rmi.RemoteException if something wrong happens during
	 *	the RMI process. Read about RMI for further explanations.
	 */
	public void save(String filename) throws RemoteException
	{
		try
		{
			ObjectOutputStream out = new ObjectOutputStream(
				new FileOutputStream(filename));
			out.writeObject(entries);
			out.close();
		}
		catch (IOException e)
		{
			System.out.println("PhoneBookServer error: " +
				e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Restores the phone book from the specified file. Deserialization
	 * is used for the process.
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
		ClassNotFoundException
	{
		try
		{
			ObjectInputStream in = new ObjectInputStream(
				new FileInputStream(filename));
			entries = (Vector) in.readObject();
			in.close();
		}
		catch (IOException e)
		{
			System.out.println("PhoneBookServer error: " +
				e.getMessage());
			e.printStackTrace();
		}
	}
}
