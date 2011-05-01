package geditor.repository;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import geditor.tools.RepConfigInfo;
import geditor.tools.XMLParsers;

/**
 * Creates a new Repository, a new RMI registry and binds the repository to the RMI registry.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RepositoryLauncher
{
	public static void main(String[] args)
	{
		RepConfigInfo rcInfo = XMLParsers.parseRepConfig();

		// the rmiPort to run the RMI registry on
		int rmiPort;

		try
		{
			rmiPort = Integer.parseInt(rcInfo.getRmiPort());
		}
		catch (NumberFormatException e)
		{
			System.out.println("Couldn't convert the rmi port specified in the configuration file into a " +
				"number. Defaulting to 1099.");
			rmiPort = 1099;
		}

		try
		{
			Repository rep = new RepositoryImpl(rcInfo.getDirectoryPath());

			// try to create a new RMI registry on rmiPort and, if this fails, try to find an already
			// running one
			Registry reg = null;
			try
			{
				reg = LocateRegistry.createRegistry(rmiPort);
			}
			catch (RemoteException e)
			{
				reg = LocateRegistry.getRegistry(rmiPort);
			}

			// bind the Repository server to the name "repository" in the newly created RMI server
			reg.rebind("repository", rep);
		}
		catch (RemoteException e)
		{
			e.printStackTrace();
			System.out.println("A RemoteException has occured. Aborting execution.");
		}
	}
}
