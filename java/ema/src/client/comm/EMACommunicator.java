/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 23, 2003
 */
package client.comm;

import server.EMAServer;
import shared.EMAProperties;
import shared.EMALogger;

import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.net.MalformedURLException;

public class EMACommunicator
{
	public synchronized static EMACommunicator getInstance()
	{
		if (communicator == null)
		{
			communicator = new EMACommunicator();
		}

		return communicator;
	}

	/**
	 * Returns a reference to the remote server object or null if an error occurs.
	 */
	public EMAServer getServerReference()
	{
		EMAServer remoteRef;

		StringBuffer url = new StringBuffer("rmi://");
		String rmiServer = EMAProperties.getInstance().getProperty("server.ip");
		String rmiPort = EMAProperties.getInstance().getProperty("rmiserver.port");

		url.append(rmiServer);
		url.append(":");
		url.append(rmiPort);
		url.append("/ema-server");

		try
		{
			EMALogger.getInstance().logOtherMessage("Trying to contact the RMI server on machine " +
				rmiServer + " running on port " + rmiPort + " in order to retrieve server reference.");
			remoteRef = (EMAServer) Naming.lookup(url.toString());
			EMALogger.getInstance().logOtherMessage("RMI server successfully contacted and server " +
				"reference retrieved.");
		}
		catch (NotBoundException e)
		{
			EMALogger.getInstance().logErrorMessage("RMI server contacted but failed to retrieve server " +
				"refernece. Check that the EMA server is running.");
			return null;
		}
		catch (MalformedURLException e)
		{
			EMALogger.getInstance().logErrorMessage("Failed to contact RMI server on machine " + rmiServer +
				" running on port " + rmiPort + ". Check that the server is running and that the " +
				"relevant entries in the configuration file (server.ip and rmiserver.port) are " +
				"correct.");
			return null;
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logErrorMessage("Failed to contact RMI server on machine " + rmiServer +
				" running on port " + rmiPort + ". Check that the server is running and that the " +
				"relevant entries in the configuration file (server.ip and rmiserver.port) are " +
				"correct.");
			return null;
		}

		return remoteRef;
	}

	private EMACommunicator()
	{}

	private static EMACommunicator communicator;
}

