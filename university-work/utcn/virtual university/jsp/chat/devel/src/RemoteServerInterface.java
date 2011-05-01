package ro.utcluj.vu.chat;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * This interface is used by the main RMI chat server. This server's goal is
 * to maintain and deploy the class based servers.
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */
public interface RemoteServerInterface extends Remote
{
	/**
	 * The method returns the class based server
	 */
	ClassServerInterface getClassServer(String classID)
		throws RemoteException;
}
