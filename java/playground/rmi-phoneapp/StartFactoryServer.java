package phoneapp;

import java.rmi.*;
import java.rmi.registry.*;

/*
 * This class provides a main() function that creates a PhoneBookFactoryServer
 * and registers it with a naming server.
 */
 
public class StartFactoryServer
{
	public static void main(String args[])
	{
		try
		{
			PhoneBookFactoryServer serverObject = new
				PhoneBookFactoryServer();
			Registry reg = LocateRegistry.createRegistry(5037);
			Naming.rebind("//192.168.0.1:5037/pb-factory",
				serverObject);
			System.out.println("Server started...");
		}
		catch (Exception e)
		{
			System.out.println("StartFactoryServer error: " +
				e.getMessage());
			e.printStackTrace();
		}
	}
}
