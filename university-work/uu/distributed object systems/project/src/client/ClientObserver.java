package client;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.io.Serializable;

/**
 * This interface specifies the methods that have to be implemented in order to support the client observer
 * functionality.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 9, 2005
 */
public interface ClientObserver extends Remote, Serializable
{
	/**
	 * This method will be called by the server when the state of the observed object changes.
	 *
	 * @param appointmentId the id of the appointment that has been changed.
	 */
	public void notifyObserver(int appointmentId) throws RemoteException;
}
