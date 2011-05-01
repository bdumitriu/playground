package core.test;

/**
 *  
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 9, 2005
 */

import junit.framework.*;
import core.*;

import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.ResultSet;
import java.sql.Statement;

public class ConnectionManagerTest extends TestCase
{
	protected void setUp() throws Exception
	{
		cm = ConnectionManager.getInstance();
	}

	public void testGetConnection() throws Exception
	{
		Connection conn = cm.getConnection();

		try
		{
			CallableStatement cs = conn.prepareCall("select * from Users");
			cs.execute();
			ResultSet rs = cs.getResultSet();
			while (rs.next())
			{
				System.out.println("User " + rs.getString("loginname") + " with password " +
					rs.getString("password") + ".");
			}
			conn.close();
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}
   
	public static Test suite()
	{
		return new TestSuite(ConnectionManagerTest.class);
	}

	private ConnectionManager cm;
}