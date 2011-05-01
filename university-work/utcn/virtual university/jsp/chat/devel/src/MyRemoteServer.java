package ro.utcluj.vu.chat;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Map;
import java.util.HashMap;


/**
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */
public class MyRemoteServer extends UnicastRemoteObject
	implements RemoteServerInterface
{
	/**
	 * The attribute is a data structure that contains all the
	 * class servers.
	 */
	private Map classes;

	/**
	 * The constructor
	 */
	public MyRemoteServer() throws RemoteException
	{
		classes = new HashMap();
	}

	/**
	 * The method returns the classID associated server if one exists
	 * or it creates a new one if it doesn't.
	 */
	public ClassServerInterface getClassServer(String classID)
		throws RemoteException
	{
		if (classes.containsKey(classID))
		{
			System.out.println("Returning already " +
				"existent chat server " +
				"for " + classID + " class.");
			return (ClassServerInterface) classes.get(classID);
		}
		else
		{
			ClassServer cserver = new ClassServer(classID);
			classes.put(classID, cserver);
			System.out.println("New chat server created for class " +
				classID + ".");

			return cserver;
		}
	}

	/**
	 * Tne main method that binds the server to the localhost machine with
	 * the given name and to the given port
	 */

	public static void main(String args[])
	{
		try
		{
			int port = 1099;
			if (args.length == 1)
			{
				port = Integer.parseInt(args[0]);
			}
			MyRemoteServer obj = new MyRemoteServer();

			Registry reg = LocateRegistry.createRegistry(port);

			reg.rebind("factory", obj);

			System.out.println("Server bound to RMI registry..");

		}
		catch (RemoteException ex)
		{
			ex.printStackTrace();
		}
	}
}
