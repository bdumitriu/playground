package core;

import java.util.Properties;
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * This is a singleton class which provides a wrapper around the server configuration file. The properties you can
 * retrieve by means of getValueFor(&lt;propertyName&gt;) are defined in the server configuration file.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 7, 2005
 */
public class ServerConfig
{
	public final static String configFile = "config/db-properties.xml";

	/**
	 * Returns an instance of this class. The same instance will be returned regardless of the number of calls.
	 *
	 * @return an instance of this class.
	 */
	public static ServerConfig getInstance()
	{
		return instance;
	}

	/**
	 * Returns the value associated with <code>property</code>. The method returns null is the property does not
	 * have an associated value.
	 *
	 * @param property the name of the property you wish to retrieve.
	 * @return the value associated with <code>property</code> or null, if no such value exists.
	 */
	public String getValueFor(String property)
	{
		return properties.getProperty(property);
	}

	/**
	 * Returns the value of the "rmi.server" property.
	 *
	 * @return the value of the "rmi.server" property.
	 */
	public String getRmiServer()
	{
		return getValueFor("rmi.server");
	}

	/**
	 * Returns the value of the "rmi.port" property as an int.
	 *
	 * @return the value of the "rmi.port" property as an int.
	 */
	public int getRmiPort()
	{
		Integer portNumber = null;
		try
		{
			portNumber = new Integer(getValueFor("rmi.port"));

			if ((portNumber.intValue() < 1) || (portNumber.intValue() > 65535))
			{
				System.out.println("The rmi port number in the configuration file is ouside the (1, " +
					"65535) interval.");
				System.exit(1);
			}
		}
		catch (NumberFormatException e)
		{
			System.out.println("The rmi port number in the configuration file is not a valid number.");
			System.exit(1);
		}

		return portNumber.intValue();
	}

	/**
	 * Returns the value of the "db.server" property.
	 *
	 * @return the value of the "db.server" property.
	 */
	public String getDbServer()
	{
		return getValueFor("db.server");
	}

	/**
	 * Returns the value of the "db.port" property as an int.
	 *
	 * @return the value of the "db.port" property as an int.
	 */
	public int getDbPort()
	{
		Integer portNumber = null;
		try
		{
			portNumber = new Integer(getValueFor("db.port"));

			if ((portNumber.intValue() < 1) || (portNumber.intValue() > 65535))
			{
				System.out.println("The database server port number in the configuration file is " +
					"ouside the (1, 65535) interval.");
				System.exit(1);
			}
		}
		catch (NumberFormatException e)
		{
			System.out.println("The database server port number in the configuration file is not a valid " +
				"number.");
			System.exit(1);
		}

		return portNumber.intValue();
	}

	/**
	 * Returns the value of the "db.user" property.
	 *
	 * @return the value of the "db.user" property.
	 */
	public String getDbUser()
	{
		return getValueFor("db.user");
	}

	/**
	 * Returns the value of the "db.pass" property.
	 *
	 * @return the value of the "db.pass" property.
	 */
	public String getDbPass()
	{
		return getValueFor("db.pass");
	}

	/**
	 * Returns the value of the "db.database" property.
	 *
	 * @return the value of the "db.database" property.
	 */
	public String getDbDatabase()
	{
		return getValueFor("db.database");
	}

	private ServerConfig()
	{
		properties = new Properties();
		try
		{
			properties.loadFromXML(new BufferedInputStream(new FileInputStream(configFile)));
		}
		catch (IOException e)
		{
			System.out.println("Could not find or read from the configuration file: " + configFile + ".");
			System.exit(1);
		}
	}

	private Properties properties;

	private static ServerConfig instance = new ServerConfig();
}
