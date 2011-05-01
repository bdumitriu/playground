package ro.utcluj.vu.chat;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * This interface describers the functionality of the class based chat server.
 * Each of the virtual university classes has a chat server that is deployed
 * by the object factory.
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */

public interface ClassServerInterface extends Remote
{
	/**
	 * The method requests the login on the class server.
	 */
	boolean requestLogin(ClientInterface user) throws RemoteException;

	/**
	 * The method requests the logout action from the client
	 * onto the server.
	 */
	void requestLogout(ClientInterface user) throws RemoteException;

	/**
	 * The method sends a message from one client to all the clients
	 * logged on the same chat class based server.
	 */
	void sendPublicMessage(ClientInterface user, String messasge)
		throws RemoteException;

	/**
	 * The method sends a message from one client to a speciffic
	 * client, both of them being logged on the same chat class
	 * based server.
	 */
	void sendPrivateMessage(ClientInterface user, ClientInterface rcpt,
		String message)
		throws RemoteException;

	/**
	 * The method is used by the server itself to send to every client
	 * logged on a certain service message such as the disconnection
	 * of one client.
	 */
	void sendServiceMessage(String message)
		throws RemoteException;

	/**
	 * The method updates the user lists of every client logged on
	 * the chat class based server.
	 */
	void updateAllUserLists() throws RemoteException;

	/**
	 * The method is used to receive the message after the login procedure.
	 */
	String getLoginMessage() throws RemoteException;
}
