package core;

import org.postgresql.ds.PGPoolingDataSource;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * This is a singleton class you can use in order to get database connections. It uses connection pooling in order to
 * allow the reuse of connections for increased performance.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 7, 2005
 */
public class ConnectionManager
{
	public static ConnectionManager getInstance()
	{
		return instance;
	}

	public Connection getConnection()
	{
		Connection conn = null;
		try
		{
			conn = source.getConnection();
		}
		catch (SQLException e)
		{
			System.out.println("There was an error while trying to get a connection to the database.");
			System.exit(1);
		}
		finally
		{
			return conn;
		}
	}

	private ConnectionManager()
	{
		ServerConfig sc = ServerConfig.getInstance();

		source = new PGPoolingDataSource();
		source.setDataSourceName("PostgreSQL data source");
		source.setServerName(sc.getDbServer());
		source.setPortNumber(sc.getDbPort());
		source.setDatabaseName(sc.getDbDatabase());
		source.setUser(sc.getDbUser());
		source.setPassword(sc.getDbPass());
		source.setInitialConnections(2);
		source.setMaxConnections(10);
	}

	private PGPoolingDataSource source;

	private static ConnectionManager instance = new ConnectionManager();
}
