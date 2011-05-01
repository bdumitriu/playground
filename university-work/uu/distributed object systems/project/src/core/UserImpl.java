package core;

import static core.Granularity.*;
import core.exceptions.*;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;

/**
 * An implementation of the User interface.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 9, 2005
 */
public class UserImpl extends UnicastRemoteObject implements User, DomainObject
{
	/**
	 * Creates a new user with a default granularity of THIRTY_MINUTES.
	 */
	public UserImpl() throws RemoteException
	{
		super();
	}

	public String getAuthToken() throws RemoteException
	{
		return authToken;
	}

	public void setAuthToken(String authToken)
	{
		this.authToken = authToken;
	}

	public String getLoginName() throws RemoteException
	{
		return loginName;
	}

	public void setLoginName(String loginName)
	{
		this.loginName = loginName;
	}

	public String getName() throws RemoteException
	{
		return name;
	}

	public void setName(String name) throws RemoteException, InvalidLoginNameException
	{
		setName(name, true);
	}

	/**
	 * Sets the full name for this user to <code>name</code>.
	 *
	 * @param name the new full name.
	 * @param persistent true if you want to update the database record as well, false otherwise.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setName(String name, boolean persistent) throws InvalidLoginNameException
	{
		this.name = name;
		if (persistent)
		{
			dbUpdate();
		}
	}

	public Granularity getGranularity() throws RemoteException
	{
		return granularity;
	}

	public void setGranularity(Granularity granularity) throws RemoteException, InvalidLoginNameException
	{
		setGranularity(granularity, true);
	}

	/**
	 * Sets the granularity for this user to <code>granularity</code>.
	 *
	 * @param granularity the new granularity.
	 * @param persistent true if you want to update the database record as well, false otherwise.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setGranularity(Granularity granularity, boolean persistent) throws InvalidLoginNameException
	{
		this.granularity = granularity;
		if (persistent)
		{
			dbUpdate();
		}
	}

	public String getTitle() throws RemoteException
	{
		return title;
	}

	public void setTitle(String title) throws RemoteException, InvalidLoginNameException
	{
		setTitle(title, true);
	}

	/**
	 * Sets the title for this user to <code>title</code>.
	 *
	 * @param title the new title.
	 * @param persistent true if you want to update the database record as well, false otherwise.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setTitle(String title, boolean persistent) throws InvalidLoginNameException
	{
		this.title = title;
		if (persistent)
		{
			dbUpdate();
		}
	}

	public String getPhoneNumber() throws RemoteException
	{
		return phoneNumber;
	}

	public void setPhoneNumber(String phoneNumber) throws RemoteException, InvalidLoginNameException
	{
		setPhoneNumber(phoneNumber, true);
	}

	/**
	 * Sets the phone number for this user to <code>phoneNumber</code>.
	 *
	 * @param phoneNumber the new phone number.
	 * @param persistent true if you want to update the database record as well, false otherwise.
	 * @throws InvalidLoginNameException if the login name of this user does not identify a valid entry in the
	 *	database.
	 */
	public void setPhoneNumber(String phoneNumber, boolean persistent) throws InvalidLoginNameException
	{
		this.phoneNumber = phoneNumber;
		if (persistent)
		{
			dbUpdate();
		}
	}

	public Calendar getCalendar() throws RemoteException
	{
		return calendar;
	}

	public void setCalendar(Calendar calendar)
	{
		this.calendar = calendar;
	}

	/**
	 * Insert this user as a new user in the database.
	 *
	 * @param password the password you want to set for this user.
	 * @throws DuplicateLoginNameException if an user with the same login name already exists in the database
	 */
	public void dbInsert(String password) throws DuplicateLoginNameException
	{
		assert loginName != null : "The login name is supposed to be set.";
		assert !(loginName.equals("")) : "The login name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement checkStatement = null;
		PreparedStatement insertStatement = null;
		try
		{
			checkStatement = conn.prepareStatement(checkStatement());
			checkStatement.setString(1, loginName);
			ResultSet rs = checkStatement.executeQuery();

			if (rs.next())
			{
				try
				{
					conn.close();
				}
				catch (SQLException e)
				{
					System.out.println("An SQL exceptions occured: " + e.getMessage());
				}
				finally
				{
					throw new DuplicateLoginNameException();
				}
			}

			insertStatement = conn.prepareStatement(insertUserStatement());
			insertStatement.setString(1, loginName);
			insertStatement.setString(2, password);

			int rowCount = insertStatement.executeUpdate();
			assert rowCount == 1 : "The row count was not 1 after inserting into the Users table";

			insertStatement = conn.prepareStatement(insertUserDetailsStatement());
			insertStatement.setString(1, loginName);
			insertStatement.setString(2, name);
			insertStatement.setString(3, title);
			insertStatement.setString(4, phoneNumber);
			switch (granularity)
			{
				case FIFTEEN_MINUTES:
					insertStatement.setInt(5, 15);
					break;
				case THIRTY_MINUTES:
					insertStatement.setInt(5, 30);
					break;
				case ONE_HOUR:
					insertStatement.setInt(5, 60);
					break;
			}

			rowCount = insertStatement.executeUpdate();
			assert rowCount == 1 : "The row count was not 1 after inserting into the UserDetails table";

			// put this object in the map of loaded objects
			UserFinder.getInstance().addToMap(loginName, this);

			conn.close();
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
		}
	}

	/**
	 * Updates the user details in the database. You cannot use this method to change a user's password. Use
	 * {@link #dbChangePassword(String)} or {@link #dbChangePassword(String, String)} instead for this purpose.
	 * <br /><br />
	 * Be aware that the user login name cannot be changed. This means that if you do a setLoginName(some-new-name)
	 * and then a dbUpdate() you will probably get an error message saying that a user with some-new-name cannot be
	 * found in the database (or, even worse, if a(nother) user with that some-new-name does exist in the database,
	 * then you will end up updating the data from that other user, which is probably not what you intend).
	 *
	 * @throws InvalidLoginNameException if there is no user in the database with the login name of this object.
	 */
	public void dbUpdate() throws InvalidLoginNameException
	{
		assert loginName != null : "The login name is supposed to be set.";
		assert !(loginName.equals("")) : "The login name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement updateStatement = null;
		try
		{
			updateStatement = conn.prepareStatement(updateUserDetailsStatement());
			updateStatement.setString(1, name);
			updateStatement.setString(2, title);
			updateStatement.setString(3, phoneNumber);
			switch (granularity)
			{
				case FIFTEEN_MINUTES:
					updateStatement.setInt(4, 15);
					break;
				case THIRTY_MINUTES:
					updateStatement.setInt(4, 30);
					break;
				case ONE_HOUR:
					updateStatement.setInt(4, 60);
					break;
			}
			updateStatement.setString(5, loginName);
			if (updateStatement.executeUpdate() == 0)
			{
				try
				{
					conn.close();
				}
				catch (SQLException e)
				{
					System.out.println("An SQL exceptions occured: " + e.getMessage());
				}
				finally
				{
					throw new InvalidLoginNameException();
				}
			}
			else
			{
				// update this object in the map of loaded objects
				UserFinder.getInstance().addToMap(loginName, this);
			}

			conn.close();
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
		}
	}

	/**
	 * Changes the password of this user to <code>password</code>. If you also need to check the old password and
	 * only change the password only if the old password is known, use {@link #dbChangePassword(String, String)}
	 * instead.
	 *
	 * @param password the new password to set.
	 * @throws InvalidLoginNameException if there is no user in the database with the login name of this object.
	 */
	public void dbChangePassword(String password) throws InvalidLoginNameException
	{
		assert loginName != null : "The login name is supposed to be set.";
		assert !(loginName.equals("")) : "The login name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement updateStatement = null;
		try
		{
			updateStatement = conn.prepareStatement(updatePasswordStatement());
			updateStatement.setString(1, password);
			updateStatement.setString(2, loginName);
			if (updateStatement.executeUpdate() == 0)
			{
				try
				{
					conn.close();
				}
				catch (SQLException e)
				{
					System.out.println("An SQL exceptions occured: " + e.getMessage());
				}
				finally
				{
					throw new InvalidLoginNameException();
				}
			}

			conn.close();
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
		}
	}

	/**
	 * Changes the password of this user to <code>newPassword</code> only if the current password matches
	 * <code>oldPassword</code>.
	 *
	 * @param oldPassword the old password (used for verification).
	 * @param newPassword the new password to set if verification succeeds.
	 * @throws InvalidLoginNameException if there is no user in the database with the login name of this object.
	 * @throws WrongPasswordException if the current password of the user doesn't match <code>oldPassword</code>.
	 */
	public void dbChangePassword(String oldPassword, String newPassword)
		throws InvalidLoginNameException, WrongPasswordException
	{
		assert loginName != null : "The login name is supposed to be set.";
		assert !(loginName.equals("")) : "The login name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement updateStatement = null;
		try
		{
			updateStatement = conn.prepareStatement(updateConditionalPasswordStatement());
			updateStatement.setString(1, newPassword);
			updateStatement.setString(2, loginName);
			updateStatement.setString(3, oldPassword);
			if (updateStatement.executeUpdate() == 0)
			{
				// find out what the reason for not changing the password was
				UserImpl user;
				if ((user = UserFinder.getInstance().find(loginName)) != null)
				{
					// since the user can be retrieved, it means the login name is valid

					// sanity check ;)
					assert user == this : "the object retrieved from the loadedMap wasn't the " +
						"same as this one";

					try
					{
						conn.close();
					}
					catch (SQLException e)
					{
						System.out.println("An SQL exceptions occured: " + e.getMessage());
					}
					finally
					{
						throw new WrongPasswordException();
					}
				}
				else
				{
					try
					{
						conn.close();
					}
					catch (SQLException e)
					{
						System.out.println("An SQL exceptions occured: " + e.getMessage());
					}
					finally
					{
						throw new InvalidLoginNameException();
					}
				}
			}

			conn.close();
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
		}
	}

	/**
	 * Deletes the user identified by this user's login name from the database.
	 * <br /><br />
	 * <i>Note: this delete method will only work if the database is set up in such a way that the deletion of
	 * the user from the Users table will trigger (in cascade) the deletion of the user details from the UserDetails
	 * table.</i> 
	 *
	 * @throws InvalidLoginNameException if there is no user in the database with the login name of this object.
	 */
	public void dbDelete() throws InvalidLoginNameException
	{
		assert loginName != null : "The login name is supposed to be set.";
		assert !(loginName.equals("")) : "The login name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		PreparedStatement deleteStatement = null;
		try
		{
			deleteStatement = conn.prepareStatement(deleteStatement());
			deleteStatement.setString(1, loginName);
			if (deleteStatement.executeUpdate() == 0)
			{
				try
				{
					conn.close();
				}
				catch (SQLException e)
				{
					System.out.println("An SQL exceptions occured: " + e.getMessage());
				}
				finally
				{
					throw new InvalidLoginNameException();
				}
			}
			else
			{
				// remove this object from the map of loaded objects
				UserFinder.getInstance().removeFromMap(loginName);
			}

			conn.close();
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
		}
	}

	private static String checkStatement()
	{
		return "SELECT password" +
			" FROM Users" +
			" WHERE loginname = ?";
	}

	private static String insertUserStatement()
	{
		return "INSERT INTO Users" +
			" VALUES (?, ?)";
	}

	private static String insertUserDetailsStatement()
	{
		return "INSERT INTO UserDetails" +
			" VALUES (?, ?, ?, ?, ?)";
	}

	private static String updateUserDetailsStatement()
	{
		return "UPDATE UserDetails" +
			" SET fullname = ?, title = ?, phone = ?, granularity = ?" +
			" WHERE loginname = ?";
	}

	private static String updatePasswordStatement()
	{
		return "UPDATE Users" +
			" SET password = ?" +
			" WHERE loginname = ?";
	}

	private static String updateConditionalPasswordStatement()
	{
		return "UPDATE Users" +
			" SET password = ?" +
			" WHERE loginname = ? AND password = ?";
	}

	private static String deleteStatement()
	{
		return "DELETE FROM Users" +
			" WHERE loginname = ?";
	}

	private String authToken = "";
	private String loginName = "";
	private String name = "";
	private Granularity granularity = THIRTY_MINUTES;
	private String title = "";
	private String phoneNumber = "";
	private Calendar calendar;
}
