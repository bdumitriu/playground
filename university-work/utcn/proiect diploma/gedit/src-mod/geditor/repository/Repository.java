package geditor.repository;

import geditor.tools.Datetime;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.ArrayList;

/**
 * This is the interface that specifies the operations that an implementation of a repository should provide for its
 * clients.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */

public interface Repository extends Remote
{
	/**
	 * Commits a new set of operations to the repository. If the <code>referenceVersion</code> differs from
	 * the current version on the repository, the commit will be unsuccessful. In order to commit when such
	 * situations arise, the client has to do an update before trying to recommit and, thus, change its reference
	 * version to the current version available on the repository. This will imply a modification of its operations
	 * in order to ensure their validity in the new context.
	 * <br /><br />
	 * @param operations the list of operations to commit (the elements are expected to be of type
	 *	{@link geditor.engine.operations.Operation Operation})
	 * @param referenceVersion the version on which the operations to commit are based
	 * @return true, if commit successful (usu. this means <code>referenceVersion</code> is equal to the current
	 *	version on the repository) and false otherwise (i.e. <code>referenceVersion</code> differs from the
	 *	current version on the repository
	 * @throws VersionException if the <code>referenceVersion</code> is negative or if it is greater than the
	 *	current version on the repository
	 * @throws RepositoryException if the repository encounters an error while trying to commit the operations
	 */
	public boolean commit(ArrayList operations, int referenceVersion)
		throws RemoteException, VersionException, RepositoryException;

	/**
	 * Returns a list of operations from the repository. This list contains the operations that need to be
	 * executed in order to change from version <code>fromVersion</code> to version <code>toVersion</code>.
	 * <br /><br />
	 * @param fromVersion the lower version
	 * @param toVersion the upper version
	 * @return a list of operations (the elements are of type {@link geditor.engine.operations.Operation Operation})
	 * @throws VersionException if either of the following is true: fromVersion > toVersion, fromVersion < 0,
	 *	fromVersion > currentVersion, toVersion > currentVersion
	 * @throws RepositoryException if one of the version files from which the operations are read is missing or
	 *	is corrupt
	 */
	public ArrayList getOperations(int fromVersion, int toVersion)
		throws RemoteException, VersionException, RepositoryException;

	/**
	 * Returns the current version on the repository.
	 * <br /><br />
	 * @return the current version on the repository
	 */
	public int getCurrentVersion() throws RemoteException;

	/**
	 * Returns the number of the version which is the closest to the <code>timestamp</code> specified from those
	 * versions that have a timestamp that's higher (i.e. older) than the one specified.
	 * <br /><br />
	 * @param timestamp the timestamp
	 * @return the version number or -1 if timestamp is higher than those of all version currently in the repository
	 */
	public int getVersionByTimestamp(Datetime timestamp) throws RemoteException;
}