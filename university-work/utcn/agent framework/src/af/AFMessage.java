package af;

import java.io.Serializable;

/**
 * This class provides access to relevant data in a message.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public interface AFMessage extends java.io.Serializable
{
	/**
	 * A constant for the type of message used in client requests.
	 */
	public static int AF_REQUEST = 1000;

	/**
	 * A constant for the type of message used in server replies.
	 */
	public static int AF_REPLY = 1001;

	/**
	 * A constant for the type of message used in server acknowledges.
	 */
	public static int AF_ACKNOWLEDGE = 1002;

	/**
	 * Sets the content of this message to <code>content</code>.
	 *
	 * @param content the content of this message.
	 */
	public void setContent(String content);

	/**
	 * Returns the content of this message.
	 *
	 * @return the content of this message.
	 */
	public String getContent();

	/**
	 * Sets the content of this message to the serialization of the
	 * <code>content</code> object.
	 *
	 * @param content the object to serialize in order to fill the content
	 * of this message.
	 * @return true if all went well; false otherwise.
	 */
	public boolean setObjectContent(Serializable content);

	/**
	 * Returns the content of this message as an object (using deserialization).
	 *
	 * @return the content of this message as an object (using deserialization).
	 * The return value could be null if something goes wrong (i.e. the content
	 * of the message does not represent a serialization of an object).
	 */
	public Serializable getObjectContent();

	/**
	 * Sets the sender of this message to <code>sender</code>.
	 *
	 * @param sender the sender of this message.
	 */
	public void setSender(AFAgentID sender);

	/**
	 * Returns the sender of this message.
	 *
	 * @return the sender of this message.
	 */
	public AFAgentID getSender();

	/**
	 * Adds a new receiver for this message.
	 *
	 * @param receiver the receiver to be added.
	 */
	public void addReceiver(AFAgentID receiver);

	/**
	 * Removes a reciever of this message.
	 *
	 * @param receiver the receiver to be removed.
	 */
	public void removeReceiver(AFAgentID receiver);

	/**
	 * Returns an array containing all the receivers of this message.
	 *
	 * @return an array containing all the receivers of this message.
	 */
	public AFAgentID[] getReceivers();

	/**
	 * Sets the type of the message to <code>type</code>.
	 *
	 * @param type the type of the message.
	 */
	public void setType(int type);

	/**
	 * Returns the type of the message.
	 *
	 * @return the type of the message.
	 */
	public int getType();

	/**
	 * Returns true if message is null and false if it is a non-null message.
	 *
	 * @return true if message is null and false if it is a non-null message.
	 */
	public boolean isNull();
}