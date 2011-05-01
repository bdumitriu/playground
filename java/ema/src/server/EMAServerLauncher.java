package server;

import shared.EMALogger;
import shared.EMAProperties;

import java.rmi.RemoteException;
import java.rmi.Naming;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.net.MalformedURLException;

/**
 * This class contains the main method to launch the EMAServer.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public class EMAServerLauncher
{
	public static void main(String[] args)
	{
		try
		{
			EMAProperties props = EMAProperties.getInstance();
			EMALogger logger = EMALogger.getInstance();

			Integer portNumber = null;
			try
			{
				portNumber = new Integer(props.getProperty("rmiserver.port"));

				if ((portNumber.intValue() < 1) || (portNumber.intValue() > 65535))
				{
					logger.logConfigMessage("The RMI server port number was a valid number, but " +
						"it was ouside the (1, 65535) interval.");
					throw new NumberFormatException();
				}
			}
			catch (NumberFormatException e)
			{
				logger.logErrorMessage("The port number you supplied by means of \"rmiserver.port\" " +
					"in the configuration file, namely \"" + props.getProperty("dbserver.port") +
					"\", was not a valid port number.");

				System.exit(1);
			}

			EMAServerImpl server = new EMAServerImpl();

			logger.logOtherMessage("Trying to create new RMI server on port " + portNumber + ".");
			Registry reg = LocateRegistry.createRegistry(2500);
			logger.logOtherMessage("RMI server started successfully.");

			// bind the EMA server to the name "ema-server" in the newly created RMI server
			reg.rebind("ema-server", server);
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logOtherMessage("Failed to start RMI server.");
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			System.out.println("Server error. Please consult the log file for details.");
			System.exit(1);
		}
	}
}
