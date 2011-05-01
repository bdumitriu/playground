package client.exception;

/**
 * The exception thrown when communication with the server fails.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class CommunicationException extends Exception
{
	public CommunicationException(String message)
	{
		super(message);
	}
}
