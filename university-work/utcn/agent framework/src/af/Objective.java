package af;

import java.util.Comparator;
import java.io.Serializable;

/**
 * This class wraps around a location used by the
 * <code>IineraryBehaviour</code>. It is just a container class.
 *
 * Date: 26.05.2003
 * Time: 10:05:39
 * @author Tudor Marian,
 * @author email tudorm@coned.utcluj.ro
 * @version 0.1
 */

public class Objective implements Comparator, Serializable
{
	/**
	 * The location this objective refers to.
	 */
	private AFLocation location;

	/**
	 * The earliest time unit relative to the <code>timePerid</code>
	 * of the itinerary class this agent is supposed to get to the
	 * location.
	 */
	private long earliest;

	/**
	 * The latest time unit relative to the <code>timePerid</code>
	 * of the itinerary class this agent is supposed to get to the
	 * location.
	 */
	private long latest;

	/**
	 * The constructors.
	 */

	/**
	 * This one is used only as a comparator implementor.
	 */
	public Objective()
	{
		earliest = -1;
		latest = -1;
		location = null;
	}

	public Objective(AFLocation location, long earliest, long latest)
		throws IllegalTimeIntervalException
	{
		if (earliest < 0)
			throw new IllegalTimeIntervalException(
			        "Earlist value is negative, supposed to be positive"
			);

		if (latest < 0)
			throw new IllegalTimeIntervalException(
			        "Latest value is negative, supposed to be positive"
			);

		if (earliest > latest)
			throw new IllegalTimeIntervalException(
			        "Earliest value greater than latest value"
			);

		this.location = location;
		this.earliest = earliest;
		this.latest = latest;
	}

	public AFLocation getLocation()
	{
		return location;
	}

	public long getEarliest()
	{
		return earliest;
	}

	public long getLatest()
	{
		return latest;
	}

	/**
	 * The method must be consistent with the <code>equals</code> one.
	 * @param o1
	 * @param o2
	 * @return
	 */
	public int compare(Object o1, Object o2)
	{
		Objective objective1 = (Objective) o1;
		Objective objective2 = (Objective) o2;

		if (objective1.getEarliest() < objective2.getEarliest())
			return -1;
		else if (objective1.getEarliest() > objective2.getEarliest())
			return 1;
		else // same earliest
		{
			if (objective1.getLatest() < objective2.getLatest())
				return -1;
			else if (objective1.getLatest() > objective2.getLatest())
				return 1;
			else
			{
				if (objective1.getLocation().equals(objective2.getLocation()))
					return 0;
				else
					return -1;
			}
		}
	}

	public boolean equals(Object o)
	{
		if (this == o)
			return true;
		if (!(o instanceof Objective))
			return false;

		final Objective objective = (Objective) o;

		if (earliest != objective.earliest)
			return false;
		if (latest != objective.latest)
			return false;
		if (location != null ? !location.equals(objective.location) : objective.location != null)
			return false;

		return true;
	}
}
