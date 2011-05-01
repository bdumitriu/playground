package client;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Observer;

/**
 * An implementation of ClientObserver.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 9, 2005
 */
public class ClientObserverImpl extends UnicastRemoteObject implements ClientObserver
{
	/**
	 * Creates a new ClientObserverImpl that will notify <code>localObserver</code> whenever it itself is notified
	 * by the server.
	 *
	 * @param localObserver the local observer that is to be notified.
	 */
	public ClientObserverImpl(Observer localObserver) throws RemoteException
	{
		super();
		obs = localObserver;
	}

	/**
	 * See documentation for {@link ClientObserver#notifyObserver(int)}.
	 */
	public void notifyObserver(int appointmentId) throws RemoteException
	{
		obs.update(null, appointmentId);
	}

	private Observer obs;
}

