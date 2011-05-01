package test;

import server.DBInterfaceManager;

import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.SQLException;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jan 6, 2004
 */
public class TestInsertPF
{
	public static void main(String[] args)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call insertPF(?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);
			stmt.setString(2, "Bogdan Dumitriu 3");
			stmt.setString(3, "Str. Malinului nr. 11, 400475, Cluj-Napoca, Romania");
			stmt.setString(4, "440058");

			stmt.execute();

			result = stmt.getInt(1);

			System.out.println("Result is: " + result + ".");
		}
		catch (SQLException e)
		{
			e.printStackTrace();
		}
		finally
		{
			DBInterfaceManager.getInstance().close(conn);
		}
	}
}
