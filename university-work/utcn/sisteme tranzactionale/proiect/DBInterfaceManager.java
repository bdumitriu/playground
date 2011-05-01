/**
 * This class is a wrapper over JDBC which simply offers connections to the database.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 03, 2004
 */

import com.microsoft.jdbcx.sqlserver.SQLServerDataSource;

import java.sql.Connection;
import java.sql.SQLException;

public class DBInterfaceManager
{
	private static DBInterfaceManager dbManager;

	public synchronized static DBInterfaceManager getInstance(String server, String user, String password)
	{
		dbManager = new DBInterfaceManager(server, user, password);

		return dbManager;
	}

	public synchronized static DBInterfaceManager getInstance()
	{
		return dbManager;
	}

	/**
	 * Returns a valid connection to the database.
	 *
	 * @return a valid connection to the database
	 */
	public Connection getConnection()
	{
		Connection connection = null;

		try
		{
			System.out.println("Trying to establish a connection to the database.");
			connection = jdbcConnection.getConnection();
			System.out.println("Connection established successfully.");
		}
		catch (SQLException e)
		{
			System.out.println("Could not manage to connect to the database server. Please check that " +
				"all parameters are correct and that the database server is running. The error " +
				"message was: \"" + e.getMessage() + "\".");
			System.exit(1);
		}

		return connection;
	}

	/**
	 * Closes the connection <code>conn</code>.
	 *
	 * @param conn the connection to close
	 */
	public void close(Connection conn)
	{
		try
		{
			System.out.println("Trying to close connection to database.");
			conn.close();
			System.out.println("Connection to database closed successfully.");
		}
		catch (SQLException e)
		{
			System.out.println(e.getMessage());
		}
	}

	/**
	 * Tries to create a data source for database connections.
	 *
	 * @param server the IP or name of the server to connect to
	 * @param user the username to use
	 * @param password the password to use
	 */
	private DBInterfaceManager(String server, String user, String password)
	{
		jdbcConnection = new SQLServerDataSource();

		System.out.println("Starting to configure the connection to the database server.");
		jdbcConnection.setDataSourceName("ST data source");

		System.out.println("Setting database server ip to " + server + ".");
		jdbcConnection.setServerName(server);

		System.out.println("Setting database server port to 1433.");
		jdbcConnection.setPortNumber(1433);

		System.out.println("Setting database name to BoxOffice.");
		jdbcConnection.setDatabaseName("BoxOffice");

		System.out.println("Setting database user to " + user + ".");
		jdbcConnection.setUser(user);

		System.out.println("Setting database password to " + password + ".");
		jdbcConnection.setPassword(password);
	}

	/**
	 * The db server connection pool.
	 */
	private SQLServerDataSource jdbcConnection;
}

