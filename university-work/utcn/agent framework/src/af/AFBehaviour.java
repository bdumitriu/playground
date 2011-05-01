package af;

import java.io.Serializable;

/**
 * This is the superclass of all Agent Framework behaviours.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class AFBehaviour
        implements Serializable
{
	/**
	 * The agent this behaviour belongs to.
	 */
	protected AFAgent agent;

	/**
	 * If true, behaviour is currently running.<br />
	 * If false, behaviour is currently suspended.
	 */
	protected boolean started;

	/**
	 * If true, behaviour has already run (at least once).<br />
	 * If false, behaviour hasn't run yet.
	 */
	protected boolean hasRun;

	/**
	 * The name of this behaviour.
	 */
	protected String name;

	public AFBehaviour()
	{
		agent = null;
		started = false;
		hasRun = false;
		name = null;
	}

	/**
	 * This is internal. Do not use.
	 */
	public boolean start()
	{
		hasRun = true;
		return true;
	}

	/**
	 * This is internal. Do not use.
	 */
	public void stop()
	{
		// do nothing
	}

	/**
	 * Call this to temporarily suspend this behaviour.
	 */
	public void suspend()
	{
		// do nothing
	}

	/**
	 * Call this to resume this behaviour.
	 */
	public void resume()
	{
		// do nothing
	}

	/**
	 * This is internal. Do not use.
	 */
	public boolean canBeRestarted()
	{
		return true;
	}

	/**
	 * This is internal. Do not use.
	 */
	public boolean isStarted()
	{
		return started;
	}

	/**
	 * Set the agent this behaviour belongs to to <code>agent</code>.<br /><br />
	 * <b>Note</b>: all this method does is store the <code>agent</code> object
	 * internally. It does not register this behaviour with the <code>agent</code>.
	 * In order to do that, use the {@link AFAgent#removeAFBehaviour} method.
	 *
	 * @param agent the agent this behaviour belongs to.
	 */
	public void setAgent(AFAgent agent)
	{
		this.agent = agent;
	}

	/**
	 * Returns the agent this behaviour belongs to.<br /><br />
	 * <b>Note</b>: all this method does is return the value stored using the
	 * {@link #setAgent} method.
	 *
	 * @return the agent this behaviour belongs to.
	 */
	public AFAgent getAgent()
	{
		return agent;
	}

	/**
	 * This is internal. Do not use.
	 */
	public boolean hasRun()
	{
		return hasRun;
	}

	/**
	 * Sets the name of this behaviour to <code>name</code>.
	 *
	 * @param name the name of this behaviour.
	 */
	public void setName(String name)
	{
		this.name = name;
	}

	/**
	 * Returns the name of this behaviour.
	 *
	 * @return the name of this behaviour.
	 */
	public String getName()
	{
		return name;
	}
}