package ro.utcluj.vu.chat;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Vector;


/**
 * This class represents the chat server for a given class. Each class such as this
 * one is deployed by the object factory.
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */


public class ClassServer extends UnicastRemoteObject
	implements ClassServerInterface
{
	/**
	 * A local String message.
	 */
	private String loginMessage = null;

	/**
	 * a user.
	 */
	private ClientInterface aUser;

	/**
	 * The class ID attribute, it identifies uniquely the server.
	 */
	private String classID;

	/**
	 * The list of clients logged on the class server.
	 */
	private Vector clients;

	/**
	 * The status after login.
	 */
	private boolean status = false;

	/**
	 * The constructor - this server will be deployed by the object-factory
	 * server.
	 */
	public ClassServer(String classID) throws RemoteException
	{
		super();
		this.classID = classID;
		clients = new Vector();
	}

	/**
	 * The method returns the class ID attribute.
	 */
	public String getClassID()
	{
		return classID;
	}

	/**
	 * The method sets the class ID attribute.
	 */
	public void setClassID(String classID)
	{
		this.classID = classID;
	}

	/**
	 * The method is the implementation of the method with
	 * the same name given into the interface's description.
	 */
	public boolean requestLogin(ClientInterface user)
		throws RemoteException
	{

		aUser = user;
		synchronized(clients)
		{
			if (!(this.contains(user)))
			{
				clients.add(user);
				loginMessage = new String("Logged in succesfully.\n");
				status = true;
				return true;
			}
			else
			{

				loginMessage = new String("You are already logged on in this class \n" +
					"from another browser session.\n");
				status = false;
				return false;
			}
		}
	}
	public String getLoginMessage()
		throws RemoteException
	{
		if (status)
		{
			updateAllUserLists();
			sendServiceMessage("user " + aUser.getUserName() + " has loogged on.");
		}
		return loginMessage;
	}
	/**
	 * The method checks if the user ID is allready taken.
	 */
	private boolean contains(ClientInterface user)
		throws RemoteException
	{
		ClientInterface element;
		for (int i = 0; i < clients.size(); i++)
		{
			element = (ClientInterface) clients.get(i);

			if (user.getUserID().equals(element.getUserID()))
				return true;
		}
		return false;
	}


	/**
	 * This method is the implementation of the method with the same
	 * name from the implemented interface.
	 */
	public void updateAllUserLists() throws RemoteException
	{
		synchronized(clients)
		{
			ClientInterface client;
			for (int i = 0; i < clients.size(); i++)
			{
				client = (ClientInterface) clients.get(i);

				client.updateUserList(clients);
			}
		}
	}


	/**
	 * This method is the implementation of the method with the same name
	 * given into the implemented interface.
	 */
	public void requestLogout(ClientInterface user)
		throws RemoteException
	{

		synchronized(clients)
		{
			if (clients.contains(user))
			{
				user.receiveServiceMessage("Logging out.");
				clients.remove(user);
				sendUserDisconnected(user);
			}
		}

		sendServiceMessage("user " + user.getUserName() + " has logged out.");
		user = null;
		updateAllUserLists();
	}

	/**
	 * The method sends to all the users the message that a user
	 * got disconnected.
	 */
	public void sendUserDisconnected(ClientInterface user)
		throws RemoteException
	{
		ClientInterface client;
		synchronized(clients)
		{
			for (int i = 0; i < clients.size(); i++)
			{
				client = (ClientInterface) clients.get(i);

				client.receiveUserDisconnected(user);
			}
		}
	}

	/**
	 * This method sends a servive message to all the clients.
	 */

	public void sendServiceMessage(String message)
		throws RemoteException
	{
		ClientInterface client;
		synchronized(clients)
		{
			for (int i = 0; i < clients.size(); i++)
			{
				client = (ClientInterface) clients.get(i);

				client.receiveServiceMessage(message);
			}
		}
	}


	/**
	 * This method sends a public message to every user logged on
	 * at a certain time.
	 */
	public void sendPublicMessage(ClientInterface user, String message)
		throws RemoteException
	{

		ClientInterface client;
		synchronized(clients)
		{
			for (int i = 0; i < clients.size(); i++)
			{
				client = (ClientInterface) clients.get(i);

				//if (!(client.getUserID().equals(user.getUserID())))
				//{
				client.receivePublicMessage(user, message);
				//}
			}
		}
	}


	/**
	 * This method sends a private message from one user to another.
	 */
	public void sendPrivateMessage(ClientInterface user, ClientInterface rcpt,
		String message)
		throws RemoteException
	{

		synchronized(clients)
		{
			if (clients.contains(rcpt))
			{
				rcpt.receivePrivateMessage(user, message);
			}
		}
	}
}
