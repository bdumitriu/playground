package chatAdmin.server;

import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;
import java.net.MalformedURLException;
import java.io.*;
import chatAdmin.*;
import java.util.Vector;

/**
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@yahoo.com
 * @version 1.0
 */
public class ChatAdminManagerServer extends java.rmi.server.UnicastRemoteObject
	implements ChatAdminManager
{
	private String host;
	private String port;
	private String password;
	private ChatAdminServer server;

	public ChatAdminManagerServer(String host, String port) 
		throws RemoteException, NumberFormatException, 
		InvalidPortException
	{
		this.host = host;
		int p = Integer.parseInt(port);
		if ((p <= 0) || (p > 60000))
			throw new InvalidPortException();
		this.port = port;
		try
		{
			ObjectInputStream inUser = new ObjectInputStream(
				new FileInputStream(
				"chatAdmin/server/data/users.dat"));
			Vector users = (Vector) inUser.readObject();
			int idx = users.indexOf(new User("admin", ""));
			if (idx != -1)
			{
				User admin = (User) users.elementAt(idx);
				password = admin.getPassword();
			}
		}
		catch (InvalidClassException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (OptionalDataException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (ClassNotFoundException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (FileNotFoundException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (StreamCorruptedException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

	public void startServer(String password) throws RemoteException,
		InvalidPasswordException
	{
		if (!(password.equals(this.password)))
			throw new InvalidPasswordException("Wrong password " +
				"for user admin.");
		try
		{
			server = new ChatAdminServer();
			Naming.rebind("//" + host + ":" + port +
				"/chatAdminServ", server);
		}
		catch (RemoteException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (MalformedURLException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

	public void stopServer(String password) throws RemoteException,
		InvalidPasswordException
	{
		if (!(password.equals(this.password)))
			throw new InvalidPasswordException("Wrong password " +
				"for user admin.");
		try
		{
			Naming.unbind("//" + host + ":" + port +
				"/chatAdminServ");
			if (server != null)
			{
				server.stopServer();
				UnicastRemoteObject.unexportObject(server, 
					true);
			}
			server = null;
		}
		catch (NoSuchObjectException e)
		{}
		catch (MalformedURLException ex)
		{
			System.out.println(ex.getMessage());
			ex.printStackTrace();
		}
		catch (NotBoundException ex)
		{}
		catch (RemoteException ex)
		{
			System.out.println(ex.getMessage());
			ex.printStackTrace();
		}
	}

	public void restartServer(String password) throws RemoteException,
		InvalidPasswordException
	{
		stopServer(password);
		startServer(password);
	}
	
	public boolean isStopped()
	{
		try
		{
			Naming.lookup("//" + host + ":" + port +
				"/chatAdminServ");
		}
		catch (Throwable e)
		{
			return true;
		}
		return false;
	}
	
	public void start()
	{
		try
		{
			startServer(password);
		}
		catch (Throwable ex)
		{
			System.out.println(ex.getMessage());
			ex.printStackTrace();
		}
	}
	
	public static void main(String args[])
	{
		try
		{
			int port = 2500;
			String stringPort = new Integer(port).toString();
			String host = "192.168.0.1";
			Registry reg = null;
			try
			{
				reg = LocateRegistry.createRegistry(port);
			}
			catch (ExportException e)
			{
				reg = LocateRegistry.getRegistry(port);
			}
			ChatAdminManagerServer server = 
				new ChatAdminManagerServer(host, stringPort);
			Naming.rebind("//" + host + ":" + stringPort +
				"/chatAdminManagerServ", server);
			server.start();
		}
		catch (RemoteException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (InvalidPortException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (MalformedURLException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}
}