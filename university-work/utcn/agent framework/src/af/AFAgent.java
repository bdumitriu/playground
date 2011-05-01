package af;

/**
 * This interface defines the generic agent this framework works with.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public interface AFAgent
        extends java.io.Serializable
{
	/**
	 * This should not be implemented by the adapter. It should be called in
	 * the adapter, however, in order to initialize the agent. This method should
	 * implemented in the classes extending the adapter.
	 *
	 * @return true if initialization successful, false otherwise.
	 */
	public boolean initialize();

	/**
	 * Attaches a new behaviour to this agent.
	 *
	 * @param behaviour the behaviour to be attached.
	 */
	public void addAFBehaviour(AFBehaviour behaviour);

	/**
	 * Removes a behaviour from the agent.
	 *
	 * @param behaviour the behaviour to be removed.
	 */
	public void removeAFBehaviour(AFBehaviour behaviour);

	/**
	 * Sends the <code>message</code>.
	 *
	 * @param message the message to send.
	 */
	public void afSend(AFMessage message);

	/**
	 * Receives a message. If no message is available, the method returns
	 * without blocking.
	 *
	 * @return the received message. If the {@link AFMessage#isNull} method
	 *      of the returned message returns true then no message was available.
	 */
	public AFMessage afReceive();

	/**
	 * Receives a message with a certain type, ignoring all others. If no
	 * message with that type is available, the method returns without blocking.
	 *
	 * @param type the type of the message to receive.
	 *
	 * @return the received message. If the {@link AFMessage#isNull} method
	 *      of the returned message returns true then no message was available.
	 */
	public AFMessage afReceive(int type);

	/**
	 * Receives a message. If no message is available, it blocks until one becomes
	 * available.
	 *
	 * @return the received message.
	 */
	public AFMessage afBlockingReceive();

	/**
	 * Receives a message. If no message is available, it blocks until one becomes
	 * available for a maximum time of <code>millis</code> milliseconds.
	 *
	 * @param millis the maximum period of time (in milliseconds) to wait for the
	 *      message.
	 *
	 * @return the received message. If the {@link AFMessage#isNull} method
	 *      of the returned message returns true then no message became available
	 *      in the specified period of time.
	 */
	public AFMessage afBlockingReceive(long millis);

	/**
	 * Receives a message with a certain type, ignoring all others. If no message
	 * is available, it blocks until one becomes available.
	 *
	 * @return the received message.
	 */
	public AFMessage afBlockingReceive(int type);

	/**
	 * Receives a message with a certain type, ignoring all others. If no message
	 * is available, it blocks until one becomes available for a maximum time of
	 * <code>millis</code> milliseconds.
	 *
	 * @param millis the maximum period of time (in milliseconds) to wait for the
	 *      message.
	 * @param type the type of the message to receive.
	 * @return
	 */
	public AFMessage afBlockingReceive(long millis, int type);

	/**
	 * Returns an empty message appropriate for the adapted platform.
	 * Implementation should be something like:<br /><br />
	 * <code>
	 * {
	 *      return new PlatformMessage();
	 * }
	 * </code>
	 *
	 * @return an empty message appropriate for the adapted platform.
	 */
	public AFMessage createEmptyMessage();

	/**
	 * Returns an AFAgentId identifying this agent.
	 *
	 * @return an AFAgentId identifying this agent.
	 */
	public AFAgentID getAgentID();

	/**
	 * Causes this agent to migrate from its current location to <code>location</code>.
	 *
	 * @param location the destination this agent is supposed to migrate to
	 */
	public void migrate(AFLocation location);

	/**
	 * Returns the current location of this agent.
	 */
	public AFLocation getCurrentLocation();
}