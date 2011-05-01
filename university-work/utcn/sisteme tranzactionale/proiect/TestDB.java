import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Jan 3, 2004
 * Time: 7:21:08 PM
 * To change this template use Options | File Templates.
 */
public class TestDB
{
	public static void main(String[] args)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();

		try
		{
			CallableStatement stmt = conn.prepareCall("{call spGetVotes()}");
			ResultSet rs = stmt.executeQuery();

			while (rs.next())
			{
				System.out.println(rs.getString(1) + "\t" + rs.getString(2) + "\t" + rs.getFloat(3) +
					"\t" + rs.getInt(4));
			}
		}
		catch (SQLException e)
		{
			System.out.println(e.getMessage());
			DBInterfaceManager.getInstance().close(conn);
			return;
		}

		DBInterfaceManager.getInstance().close(conn);
	}
}
