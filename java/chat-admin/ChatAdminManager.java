package chatAdmin;

import java.rmi.*;

/**
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@yahoo.com
 * @version 1.0
 */
public interface ChatAdminManager extends Remote
{
	public void startServer(String password) throws RemoteException,
		InvalidPasswordException;
	
	public void stopServer(String password) throws RemoteException,
		InvalidPasswordException;

	public void restartServer(String password) throws RemoteException,
		InvalidPasswordException;
	
	public boolean isStopped() throws RemoteException;
}