import java.sql.*;
import org.gjt.mm.mysql.Driver;

public class MysqlConnection
{
	// Enter the IP address of the computer where the database is running on here.
	private static final String DATABASE_IP_ADDRESS = "localhost";
	
	// Enter the type of the database here. MySQL in this case.
	private static final String DATABASE_TYPE = "mysql";
	
	// Enter the name of the database here.
	private static final String DATABASE_NAME = "doc";
	
	// Enter the user for the database here.
	private static final String DATABASE_USER = "root";
	
	// Enter the password of the user here.
	private static final String DATABASE_PASSWORD = "aaaaaa";
	
	
	/**
	 * Connects to the databses and prints a complete column of a specified table to System.out
	 * Use this method for example like this:
	 *
	 * printTableColumn("atable", "acolumn");
	 *
	 */
	public static void printTableColumn(String table, String column)
	{
		// Load JDBC driver class.
		try
		{
			// Enter the correct driver class name here.
			// Make sure the class can be found via the "Library class directories" in the
			// "Project Settings" dialog.
			// In this case the MySQL JDBC driver is used.
			Class driverClass = org.gjt.mm.mysql.Driver.class;
		}
		catch (Exception e)
		{
			System.err.println(e);
			return ;
		}
		
		// Establish connection.
		Connection connection = null;
		try
		{
			String url = "jdbc:" + DATABASE_TYPE
				+ "://" + DATABASE_IP_ADDRESS
				+ "/" + DATABASE_NAME
				+ "?user=" + DATABASE_USER
				+ "&password=" + DATABASE_PASSWORD;
			
			connection = DriverManager.getConnection(url);
		}
		catch (SQLException e)
		{
			System.err.println(e);
			return;
		}
		
		// Read table.
		try
		{
			Statement stmt = connection.createStatement();
			String query = "SELECT * FROM " + table;
			ResultSet rs = stmt.executeQuery(query);
			
			while (rs.next())
			{
				// TODO: Do something with the data.
				System.out.println(rs.getString(column));
			}
			
			stmt.close();
		}
		catch (SQLException e)
		{
			System.err.println(e);
			return ;
		}
		
		// Close connection.
		try
		{
			connection.close();
		}
		catch (SQLException e)
		{
			System.err.println(e);
			return;
		}
	}
	
	public static void main(String args[])
	{
		printTableColumn("documentation", "title");
	}
}
