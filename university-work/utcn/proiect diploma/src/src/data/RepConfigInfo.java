package data;

/**
 * This is a container class for passing the data contained in a rep-config.xml file.
 * <br /><br />
 * Date: Feb 25, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RepConfigInfo
{
	public String getRmiPort()
	{
		return rmiPort;
	}

	public void setRmiPort(String rmiPort)
	{
		this.rmiPort = rmiPort;
	}

	public String getDirectoryPath()
	{
		return directoryPath;
	}

	public void setDirectoryPath(String directoryPath)
	{
		this.directoryPath = directoryPath;
	}

	public String getDefaultRmiServer()
	{
		return defaultRmiServer;
	}

	public void setDefaultRmiServer(String defaultRmiServer)
	{
		this.defaultRmiServer = defaultRmiServer;
	}

	/**
	 * The port on which the RMI registry should be started.
	 */
	private String rmiPort;

	/**
	 * The path to the directory to be used by the RMI server.
	 */
	private String directoryPath;

	/**
	 * The default ip of the RMI server which is displayed to the client.
	 */
	private String defaultRmiServer;
}
