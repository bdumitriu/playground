package core;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.rmi.RemoteException;

/**
 * A singleton class which functions as an identity map for calendar objects. Calendars should not be created directly,
 * but rather by using the {@link #find(String)} method of this class.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 9, 2005
 */
public class CalendarFinder extends AbstractFinder
{
	public static CalendarFinder getInstance()
	{
		if (instance == null)
		{
			instance = new CalendarFinder();
		}

		return instance;
	}

	/**
	 * Returns the calendar belonging to the user identified by <code>loginName</code>.
	 *
	 * @param loginName the name of the user who is the owner of this calendar.
	 * @return the calendar belonging to the user identified by <code>loginName</code>.
	 */
	synchronized public CalendarImpl find(String loginName)
	{
		DomainObject result = loadedMap.get(loginName);
		if (result != null)
		{
			return (CalendarImpl) result;
		}

		try
		{
			CalendarImpl cal = new CalendarImpl();
			cal.setUserName(loginName);
			loadedMap.put(loginName, cal);
			return cal;
		}
		catch (RemoteException e)
		{
			// will never be thrown, since this is a local call
			return null;
		}
	}

	protected String findStatement()
	{
		return null;
	}

	protected DomainObject doLoad(String id, ResultSet rs) throws SQLException
	{
		return null;
	}

	private CalendarFinder()
	{
		super();
	}

	private static CalendarFinder instance = null;
}
