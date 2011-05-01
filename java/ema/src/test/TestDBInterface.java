package test;

import server.DBInterfaceManager;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.ResultSet;

import shared.EMALogger;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 10, 2003
 */
public class TestDBInterface
{
	public static void main(String[] args)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();

		try
		{
			PreparedStatement stmt = conn.prepareStatement("select * from teste");
			ResultSet rs = stmt.executeQuery();
			rs.next();

			while (!rs.isAfterLast())
			{
				System.out.println(rs.getInt("id") + "\t" + rs.getString("name"));
				rs.next();
			}
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			System.exit(1);
		}
	}
}