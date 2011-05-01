package af;

/**
 * This is a one shot behaviour meant to allow an agent to wait for a particular type
 * of message. The agent will provide a method to be called by this behaviour when
 * the message arrives. Once the message is received, this behaviour will end. In order
 * to wait for another message, create a new ListenerBehaviour and register it with
 * the agent. You should also consider removing this behaviour from the agent once
 * its execution ends (i.e. your callback method is called).
 *
 * Date: May 24, 2003
 * @author Bogdan Dumitriu
 * @author email bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class ListenerBehaviour extends SingleBehaviour
{
	private AFAgent lAgent;
	private int type;
	private ListenerEvent event;

	/**
	 * Builds a new ListenerBehaviour which will make the agent wait until a specific
	 * message is received.
	 *
	 * @param agent the agent waiting for the message (typically it will be the same
	 *      agent this behaviour belongs to).
	 * @param type the type of message the agent is waiting for (use -1 for any type).
	 * @param event the ListenerEvent object whose {@link ListenerEvent#callBackWhenReceived}
	 *      method will be called when the message arrives.
	 */
	public ListenerBehaviour(AFAgent agent, int type, ListenerEvent event)
	{
		super();
		this.lAgent = agent;
		this.type = type;
		this.event = event;
	}

	public boolean start()
	{
		if (!started)
		{
			return false;
		}

		AFMessage message = null;
		if (type == -1)
		{
			message = lAgent.afReceive();
		}
		else
		{
			message = lAgent.afReceive(type);
		}

		if (!message.isNull())
		{
			event.callBackWhenReceived(message);
			hasRun = true;
		}

		return true;
	}

	public void suspend()
	{
		started = false;
	}

	public void resume()
	{
		started = true;
	}

	public boolean canBeRestarted()
	{
		return false;
	}
}