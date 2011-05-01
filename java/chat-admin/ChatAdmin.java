package chatAdmin;

import java.rmi.*;
import java.util.Vector;

/**
 * This remote interface specifies the methods available for RMI clients that
 * want to connect to a ChatAdmin server.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public interface ChatAdmin extends Remote
{
	public Vector getChatRooms(String userName, String password)
		throws RemoteException, UserNotFoundException,
		InvalidPasswordException;

	public Vector getUsers(String password) throws RemoteException,
		InvalidPasswordException;

	public User getUser(String userName, String password)
		throws RemoteException, UserNotFoundException,
		InvalidPasswordException;

	public void lockAccess(String password) throws RemoteException,
		InvalidPasswordException;

	public void unlockAccess(String password) throws RemoteException,
		InvalidPasswordException;
	
	public boolean isLocked() throws RemoteException;

	public void submitChanges(String userName, String password,
		Vector newChatRooms, Vector newUsers) throws RemoteException,
		UserNotFoundException, InvalidPasswordException, LockException;
}
