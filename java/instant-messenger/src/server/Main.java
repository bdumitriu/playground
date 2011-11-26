package server;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class Main
{
	public static void main(String[] args)
	{

		if (args.length != 1)
		{
			System.out.println("Usage: java Main <port>");
			System.exit(0);
		}

		UserManager um = UserManager.getInstance();
		IMServer server = new IMServer(Integer.parseInt(args[0]));
		server.run();
/*
		UserManager um = UserManager.getInstance();
		um.saveUsers();
*/
	}
}
