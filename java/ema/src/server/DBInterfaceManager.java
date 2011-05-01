/**
 * This class is a wrapper over JDBC which simply offers connections to the database.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 10, 2003
 */
package server;

import shared.EMAProperties;
import shared.EMALogger;

import org.postgresql.jdbc3.Jdbc3PoolingDataSource;

import java.sql.Connection;
import java.sql.SQLException;

public class DBInterfaceManager
{
	private static DBInterfaceManager dbManager;

	public synchronized static DBInterfaceManager getInstance()
	{
		if (dbManager == null)
		{
			dbManager = new DBInterfaceManager();
		}

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
		EMALogger logger = EMALogger.getInstance();

		try
		{
			logger.logOtherMessage("Trying to establish a connection to the database.");
			connection = jdbcConnectionPool.getConnection();
			logger.logOtherMessage("Connection established successfully.");
		}
		catch (SQLException e)
		{
			logger.logErrorMessage("Could not manage to connect to the database server. Please check that" +
				" all parameters supplied in the configuration file are correct and that the database" +
				" server is running. The error message was: \"" + e.getMessage() + "\".");

			System.exit(1);
		}

		return connection;
	}

	/**
	 * Closes the connection <code>conn</code>.
	 */
	public void close(Connection conn)
	{
		try
		{
			EMALogger.getInstance().logOtherMessage("Trying to close connection to database.");
			conn.close();
			EMALogger.getInstance().logOtherMessage("Connection to database closed successfully.");
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
		}
	}

	/**
	 * Tries to create a pooled connection to the database.
	 */
	private DBInterfaceManager()
	{
		jdbcConnectionPool = new Jdbc3PoolingDataSource();
		EMAProperties props = EMAProperties.getInstance();
		EMALogger logger = EMALogger.getInstance();

		logger.logConfigMessage("Starting to configure the connection to the database server.");
		jdbcConnectionPool.setDataSourceName("EMA data source");

		logger.logConfigMessage("Setting database server ip to " + props.getProperty("dbserver.ip") + ".");
		jdbcConnectionPool.setServerName(props.getProperty("dbserver.ip"));

		Integer portNumber = null;
		try
		{
			portNumber = new Integer(props.getProperty("dbserver.port"));

			if ((portNumber.intValue() < 1) || (portNumber.intValue() > 65535))
			{
				logger.logConfigMessage("The database server port number was a valid number, but it " +
					"was ouside the (1, 65535) interval.");
				throw new NumberFormatException();
			}
		}
		catch (NumberFormatException e)
		{
			logger.logErrorMessage("The port number you supplied by means of \"dbserver.port\" in the " +
				"configuration file, namely \"" + props.getProperty("dbserver.port") +
				"\", was not a valid port number.");

			System.exit(1);
		}

		logger.logConfigMessage("Setting database server port to " + props.getProperty("dbserver.port") + ".");
		jdbcConnectionPool.setPortNumber(portNumber.intValue());

		logger.logConfigMessage("Setting database name to " + props.getProperty("dbserver.database") + ".");
		jdbcConnectionPool.setDatabaseName(props.getProperty("dbserver.database"));

		logger.logConfigMessage("Setting database user to " + props.getProperty("dbserver.user") + ".");
		jdbcConnectionPool.setUser(props.getProperty("dbserver.user"));

		// TODO: see if password is required when connecting from another host than
		// localhost. Tests have shown that on localhost no password is required.
		logger.logConfigMessage("Setting database password to " + props.getProperty("dbserver.password") + ".");
		jdbcConnectionPool.setPassword(props.getProperty("dbserver.password"));

		logger.logOtherMessage("Setting initial number of connections to 1.");
		jdbcConnectionPool.setInitialConnections(1);

		logger.logOtherMessage("Setting maximum number of pooled connections to 5.");
		jdbcConnectionPool.setMaxConnections(5);
	}

	/**
	 * The db server connection pool.
	 */
	private Jdbc3PoolingDataSource jdbcConnectionPool;
}

