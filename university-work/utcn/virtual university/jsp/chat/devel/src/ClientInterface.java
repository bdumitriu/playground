package ro.utcluj.vu.chat;

import java.util.Vector;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * This interface provides the methods for the callback procedure. They
 * are used by the server to "tell" the client
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */

public interface ClientInterface extends Remote
{
	/**
	 * The method returns the user ID attribute
	 */
	String getUserID() throws RemoteException;

	/**
	 * The method returns the user name attribute
	 */
	String getUserName() throws RemoteException;

	/**
	 * The method updates the user list of the client
	 */
	void updateUserList(Vector list)
		throws RemoteException;

	/**
	 * The method displays a public message
	 */
	void receivePublicMessage(ClientInterface from, String message)
		throws RemoteException;

	/**
	 * The method displays a private message
	 */
	void receivePrivateMessage(ClientInterface from, String message)
		throws RemoteException;

	/**
	 * The method displays a service message
	 */
	void receiveServiceMessage(String message)
		throws RemoteException;

	/**
	 * The method checks if this object is equal with another
	 * one of the same tipe.
	 */
	boolean equals(ClientInterface object)
		throws RemoteException;

	/**
	 * The method is used to tell that a certain user got disconnected
	 * from the server.
	 */
	void receiveUserDisconnected(ClientInterface object)
		throws RemoteException;

	/**
	 * The method indicates that the client should be inactive
	 * bacause he is already logged on into that class.
	 */
	void setInactive()
		throws RemoteException;
}
