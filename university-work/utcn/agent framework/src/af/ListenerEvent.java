package af;

/**
 * A simple interface agents who want to use {@link ListenerBehaviour}s have to
 * implement.
 *
 * Date: May 24, 2003
 * @author Bogdan Dumitriu
 * @author email bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public interface ListenerEvent extends java.io.Serializable
{
	/**
	 * This method is called by the listener behaviour when the agent receives
	 * the message it had been waiting for. This method is seen as a handler of
	 * the message receival.
	 *
	 * @param message the message received by the agent.
	 */
	public void callBackWhenReceived(AFMessage message);
}