import java.sql.*;
import javax.sql.*;
import com.jnetdirect.jsql.*;

public class DataSourceMsSQLConnection {
	public static void main(String args[]) {
		/*
			Note to self:

			The following sequence initializes a connection	to my
			Microsoft SQL Server 2000...
		*/
		JSQLDataSource jds = new JSQLDataSource();
		jds.setUser("bdumitriu");
		jds.setPassword("ihavvwg");
		jds.setDatabase("Gestiune");
		Connection con = null;

		try {
			con = jds.getConnection();
			System.out.println("connection opened...");
		} catch (SQLException e) {
			System.out.println(e.getMessage());
		}

		/*
			Note to self:

			The following piece of code sends a SQL statement to the
			server, stores the result in a ResultSet object, closes 
			the statement and proccesses the result.
		*/
		try {
			Statement stmt = con.createStatement();
			ResultSet rs = stmt.executeQuery("SELECT DISTINCT Oras " + 
							 "FROM Furnizor");
			while (rs.next()) {
				String oras = rs.getString("Oras");
				System.out.println(oras);
			}
			stmt.close();
		} catch (SQLException e) {
			System.out.println(e.getMessage());
		}

		/*
			Note to self:

			The following line closes the connection to the server.
		*/
		try {
			con.close();
			System.out.println("connection closed...");
		} catch (SQLException e) {
			System.out.println(e.getMessage());
		}
	}
}