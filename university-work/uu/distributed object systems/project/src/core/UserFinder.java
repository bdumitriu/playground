package core;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.rmi.RemoteException;
import java.util.ArrayList;

/**
 * A singleton class which provides some means of retrieving users from the database as UserImpl objects.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 9, 2005
 */
public class UserFinder extends AbstractFinder
{
	public static UserFinder getInstance()
	{
		return instance;
	}

	/**
	 * Finds and returns a user identified by <code>loginName</code>.
	 *
	 * @param loginName the login name of the user you want to find.
	 * @return the user identified by <code>loginName</code>.
	 */
	public UserImpl find(String loginName)
	{
		return (UserImpl) abstractFind(loginName);
	}

	/**
	 * Finds and returns a user identified by <code>loginName</code> only if his password matches.
	 *
	 * @param loginName the login name of the user you want to find.
	 * @return the user identified by <code>loginName</code>.
	 */
	public UserImpl findIfPassword(String loginName, String password)
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement findStatement = null;
		try
		{
			findStatement = conn.prepareStatement(checkStatement());
			findStatement.setString(1, loginName);
			ResultSet rs = findStatement.executeQuery(); 

			if (!rs.next() || !rs.getString(1).equals(password))
			{
				return null;
			}

			conn.close();

			return find(loginName);
		}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());

			return null;
		}
	}

	/**
	 * Finds and returns a list of all the users in the system. The users returned by this mehtod will not be added
	 * to the loaded users map.
	 *
	 * @return a list of all the users in the system.
	 */
	public ArrayList<UserData> findAllUsers()
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			PreparedStatement findStatement = conn.prepareStatement(findAllStatement());
			ResultSet rs = findStatement.executeQuery();

			ArrayList<UserData> users = new ArrayList<UserData>();
			while (rs.next())
			{
				users.add(new UserData(rs.getString("loginName"), rs.getString("fullname")));
			}

			conn.close();

			return users;
		}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());

			return null;
		}
	}

	/**
	 * See the documentation for {@link AbstractFinder#doLoad(String, java.sql.ResultSet)}.
	 */
	protected DomainObject doLoad(String loginName, ResultSet rs) throws SQLException
	{
		UserImpl user = null;
		try
		{
			user = new UserImpl();
			user.setLoginName(loginName);
			user.setName(rs.getString(3), false);
			user.setTitle(rs.getString(4), false);
			user.setPhoneNumber(rs.getString(5), false);
			switch (rs.getInt(6))
			{
				case 15:
					user.setGranularity(Granularity.FIFTEEN_MINUTES, false);
					break;
				case 60:
					user.setGranularity(Granularity.ONE_HOUR, false);
					break;
				default:
					user.setGranularity(Granularity.THIRTY_MINUTES, false);
					break;
			}

			// get the Calendar object for this user
			CalendarImpl cal = CalendarFinder.getInstance().find(loginName);
			user.setCalendar(cal);
		}
		catch (RemoteException e)
		{
			// there is no remote method call, so no RemoteException will be thrown
		}
		finally
		{
			return user;
		}
	}

	protected String findStatement()
	{
		return "SELECT u.loginname, password, fullname, title, phone, granularity" +
			" FROM Users u, UserDetails ud" +
			" WHERE u.loginname = ? AND u.loginname = ud.loginname";
	}

	private UserFinder()
	{
		super();
	}

	private static String checkStatement()
	{
		return "SELECT password FROM Users WHERE loginname = ?";
	}

	private String findAllStatement()
	{
		return "SELECT u.loginname AS loginname, fullname" +
			" FROM Users u, UserDetails ud" +
			" WHERE u.loginname = ud.loginname";
	}

	private static UserFinder instance = new UserFinder();
}
