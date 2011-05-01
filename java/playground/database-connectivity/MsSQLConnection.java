import java.sql.*;
import com.jnetdirect.jsql.*;

public class MsSQLConnection {
	public static void main(String args[]) {
		String url = "jdbc:JSQLConnect://blackwizard/";
		String db = "database=Gestiune/";
		String user = "user=bdumitriu/";
		String password = "password=ihavvwg";

		/*
			Note to self:

			The following try-catch sequence is required in order
			for DriverManager to register the JSQLDriver driver.
			The purpose of calling Class.forName("driver_class")
			is that by doing that the driver class is loaded and,
			normally, each driver class should statically register
			itself upon load to the DriverManager by calling its 
			registerDriver() method.
		*/
		try {
			Class.forName("com.jnetdirect.jsql.JSQLDriver");
		} catch (ClassNotFoundException e) {
			System.out.println(e.getMessage());
		}

		/*
			Note to self:

			The following try-catch sequence initializes a connection
			to my Microsoft SQL Server 2000...
		*/
		Connection con = null;

		try {
			con = DriverManager.getConnection(url + db + user + password);
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