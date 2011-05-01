package chatAdmin;

import java.rmi.*;
import chatAdmin.server.*;

/**
 * This remote interface is used for callbacks (RMI made by the server to make
 * the client aware of the changes that have been made) from a ChatAdmin server.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public interface Refresh extends Remote
{
	public void userModified(User user) throws RemoteException;

	public void userDeleted(User user) throws RemoteException;

	public void chatRoomsModified() throws RemoteException;

	public void accessLocked() throws RemoteException;

	public void serverShuttingDownIn5() throws RemoteException;

	public void serverShutDown() throws RemoteException;
}
