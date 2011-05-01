/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 6, 2003
 */
package shared;

import java.util.Properties;
import java.util.Enumeration;
import java.util.Locale;
import java.io.FileInputStream;
import java.io.IOException;

public class EMAProperties
{
	public synchronized static EMAProperties getInstance()
	{
		if (emaProps == null)
		{
			emaProps = new EMAProperties();
		}

		return emaProps;
	}

	/**
	 * Returns the value associated with the specified key or null if no
	 * mapping exists for the given key.
	 *
	 * @param key the key to index the properties hash table
	 * @return the value associated with the key
	 */
	public String getProperty(String key)
	{
		return props.getProperty(key);
	}

	/**
	 * Changes the value associated with <code>key</code> to <code>value</code>.
	 * If no previous mapping existed, one is created and the (key, value)
	 * pair is set to (<code>key</code>, <code>value</code>).
	 *
	 * @param key the key to index the properties hash table
	 * @param value the value associated with the key
	 *
	 * @throws NullPointerException if the key or value is null
	 */
	public void setProperty(String key, String value) throws NullPointerException
	{
		props.setProperty(key, value);
	}

	/**
	 * Returns the {@link java.util.Locale Locale} set up with the current values
	 * of the locale.* properties.
	 */
	public Locale getLocale()
	{
		return locale;
	}

	public String toString()
	{
		StringBuffer sb = new StringBuffer(super.toString());

		Enumeration keys = props.keys();
		while (keys.hasMoreElements())
		{
			String key = (String) keys.nextElement();

			sb.append("\n");
			sb.append(key);
			sb.append(" = ");
			sb.append(props.getProperty(key));
		}

		return sb.toString();
	}

	private EMAProperties()
	{
		props = new Properties();
		try
		{
			props.load(new FileInputStream("etc/EMA.properties"));
		}
		catch (IOException e)
		{}

		validateProperties();

		locale = new Locale(getProperty("locale.language"),
			        getProperty("locale.country"), getProperty("locale.system"));
	}

	/**
	 * Checks all systems properties for existence and validity. The ones
	 * that don't exist are added and the ones whose values aren't valid
	 * are modified. In both cases default values are used.
	 */
	private void validateProperties()
	{
		Object temp;
		String value;
		String[] keys = EMADefaultProperties.allKeys;
		String[][] values = EMADefaultProperties.allValues;

		for (int i = 0; i < keys.length; i++)
		{
			// check if key appears in properties set
			if ((temp = props.getProperty(keys[i])) == null)
			{
				// since it doesn't apper, add it
				props.setProperty(keys[i], values[i][0]);
			}
			else
			{
				// check if value is allowed
				value = (String) temp;

				// only check value for those keys for which
				// allowed values are defined
				if (values[i].length > 1)
				{
					boolean ok = false;
					for (int j = 1; j < values[i].length; j++)
					{
						if (value.equalsIgnoreCase(values[i][j]))
						{
							ok = true;
							j = values[i].length;
						}
					}
					if (ok == false)
					{
						props.setProperty(keys[i], values[i][0]);
					}
				}
			}
		}
	}

	/**
	 * The EMA system properties.
	 */
	private Properties props;

	/**
	 * The current locale.
	 */
	private Locale locale;

	/**
	 * An instance of this class.
	 */
	private static EMAProperties emaProps;
}