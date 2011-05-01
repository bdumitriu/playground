package af;

/**
 *
 *
 * @author Tudor Marian
 * @author email: tudorm@coned.utcluj.ro
 * @version 0.1
 *
 * Date: Aug 6, 2003
 */
public class ObjectiveReachedEvent
        implements java.io.Serializable
{
	/**
	 * The time base of the ItineraryBehaviur.
	 */
	private long timeBase;

	/**
	 * The objective reached upon the emergence of this event.
	 */
	private Objective objective;

	public ObjectiveReachedEvent(long timeBase, Objective objective)
	{
		this.timeBase = timeBase;
		this.objective = objective;
	}

	public long getTimeBase()
	{
		return timeBase;
	}

	public void setTimeBase(long timeBase)
	{
		this.timeBase = timeBase;
	}

	public Objective getObjective()
	{
		return objective;
	}

	public void setObjective(Objective objective)
	{
		this.objective = objective;
	}
}
