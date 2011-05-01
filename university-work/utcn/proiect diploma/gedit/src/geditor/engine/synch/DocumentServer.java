package geditor.engine.synch;

import geditor.repository.VersionException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;

import geditor.repository.VersionException;
import geditor.elements.GRootGroup;

/**
 * The interface of the RMI server used for synchronization between two users.
 */
public interface DocumentServer extends Remote
{
	/**
	 * Returns the tree representing the state of the local graphical document.
	 * <br /><br />
	 *
	 * @throws PermissionException if the client does not accept to allow the remote user to do the synchronization
	 */
	public GRootGroup getState() throws RemoteException, PermissionException;
}