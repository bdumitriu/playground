package af;

import java.util.HashMap;
import java.util.Map;

/**
 * This class encapsulates data identifying an agent.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public abstract class AFAgentID
        implements java.io.Serializable
{
	private Map properties;

	public AFAgentID()
	{
		properties = new HashMap();
	}

	/**
	 * Adds a new (<code>name</code>, <code>value</code>) pair contributing to the
	 * identification of an agent.
	 *
	 * @param name the name of the added property.
	 * @param value the value of the added property.
	 */
	public void addProperty(String name, String value)
	{
		properties.put(name, value);
	}

	/**
	 * Returns the value associated with this <code>name</code> or null if no value
	 * is associated with this name.
	 *
	 * @param name the name of the property whose value you want.
	 *
	 * @return the value associated with this <code>name</code> or null if no value
	 *      is associated with this name.
	 */
	public String getProperty(String name)
	{
		return (String) properties.get(name);
	}

	/**
	 * Returns a String id uniquely identifying this agent in a certain context.
	 *
	 * @return a String id uniquely identifying this agent in a certain context.
	 */
	public abstract String getID();
}