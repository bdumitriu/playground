package geditor.engine.synch;

import geditor.repository.VersionException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;

import geditor.repository.VersionException;

/**
 * The interface of the RMI server used for synchronization between two users.
 */
public interface DocumentServer extends Remote
{
	/**
	 * Returns the list of operations this client has in its local log.
	 *
	 * @param version document version of the caller (remote user)
	 * @throws PermissionException if the client does not accept to allow the remote user to do the synchronization
	 * @throws VersionException if the two documents the users that are synchronizing began from do not have the
	 *	same version
	 */
	public ArrayList getOperations(int version)
		throws RemoteException,  PermissionException,  VersionException;
}