package phoneapp;

import java.io.Serializable;

/**
 * This class represents the basic structure for the phone book application.
 * It is used for storing and obtaining a phone book entry (a name and a
 * number). Its functionality is made quite clear by the names of its methods.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0, 19.08.2001
 */

public class Entry implements Serializable
{
	private String name;
	private String number;
	
	/**
	 * Creates a new entry using the specified name and phone number.
	 * <br><br>
	 * @param name the name of the person.
	 * @param number the phone number of the person.
	 */
	public Entry(String name, String number)
	{
		this.name = name;
		this.number = number;
	}

	/**
	 * Returns the name part of the entry.
	 * <br><br>
	 * @return the name part of the entry
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Returns the number part of the entry.
	 * <br><br>
	 * @return the number part of the entry
	 */
	public String getNumber()
	{
		return number;
	}
	
	/**
	 * Creates a string representation of the phone book entry in the
	 * following format:
	 * <pre>
	 *	Name:		number
	 * </pre>
	 *
	 * @return the string representation.
	 */
	public String toString()
	{
		return name + "\t\t" + number;
	}
	
	/**
	 * Overrides the equals method in the Object class. This is done in
	 * order to make sure that an entry with the same name and number
	 * is considered equal to this one.
	 * <br><br>
	 * @param obj the Entry object to compare this with.
	 * @see java.lang.Object#equals(Object obj)
	 * @return true if the name & number of the two are identical and
	 *	false otherwise.
	 */
	public boolean equals(Object obj)
	{
		Entry entry = (Entry) obj;
		
		return name.equals(entry.name) && number.equals(entry.number);
	}
}
