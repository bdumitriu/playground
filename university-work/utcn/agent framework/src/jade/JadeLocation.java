package jade;

import af.AFLocation;
import jade.core.Location;

import java.io.Serializable;

/**
 * The location adapter for the Jade agent platform.
 *
 * Date: 26.05.2003
 * Time: 10:08:46
 * @author Tudor Marian,
 * @author email tudorm@c7.campus.utcluj.ro
 * @version 0.1
 */

public class JadeLocation implements AFLocation
{
	/**
	 * The Jade location object.
	 */
	Location location;

	private JadeLocation()
	{}

	public JadeLocation(Location location)
	{
		this.location = location;
	}

	public Location getLocation()
	{
		return location;
	}

	public void setLocation(Location location)
	{
		this.location = location;
	}

	public boolean equals(Object o)
	{
		if (this == o)
			return true;
		if (!(o instanceof JadeLocation))
			return false;

		final JadeLocation jadeLocation = (JadeLocation) o;

		if (location != null ? !location.equals(jadeLocation.location) : jadeLocation.location != null) return false;

		return true;
	}

	public int hashCode()
	{
		return (location != null ? location.hashCode() : 0);
	}
}
