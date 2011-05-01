package comm.dusync;

import comm.repository.VersionException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;

/**
 * This is the interface that specifies the operations that an implementation of a document server should provide
 * for its clients. A document server is used to provide direct user synchronization.
 */
public interface DocumentServer extends Remote
{
	/**
	 * Returns the list of operations of the local client in order for the requesting client to be able to
	 * synchronize directly with this client.
	 *
	 * @param version the document version of the caller
	 * @throws PermissionException if the remote user does not accept the synchronization
	 * @throws VersionException if the two documents do not have the same version
	 */
	public ArrayList getOperations(int version) throws RemoteException,  PermissionException,  VersionException;
}