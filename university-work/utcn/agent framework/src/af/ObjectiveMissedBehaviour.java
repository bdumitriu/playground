package af;

/**
 * The class represents a behaviour called upon when during the
 * execution of another <code>ItineraryBehaviour</code> an objective
 * was not reached on time.
 *
 * Date: 26.05.2003
 * Time: 09:50:19
 * @author Tudor Marian,
 * @author email tudorm@coned.utcluj.ro
 * @version 0.1
 */

public class ObjectiveMissedBehaviour extends SingleBehaviour
        implements java.io.Serializable
{
	/**
	 * The <code>ItineraryBehaviour</code> this behaviour
	 * is bound to.
	 */
	private ItineraryBehaviour contextBehaviour;

	public ItineraryBehaviour getContextBehaviour()
	{
		return contextBehaviour;
	}

	public void setContextBehaviour(ItineraryBehaviour contextBehaviour)
	{
		this.contextBehaviour = contextBehaviour;
	}

	public boolean start()
	{
		if (!super.start())
			return false;

		// the default action of this behavior is to stop migrating

		contextBehaviour.stop();

		return true;
	}

	public void suspend()
	{
		super.suspend();
	}

	public void resume()
	{
		super.resume();
	}

	/**
	 * This behaviour can not be restarted.
	 *
	 * @return false always
	 */
	public boolean canBeRestarted()
	{
		return false;
	}
}
