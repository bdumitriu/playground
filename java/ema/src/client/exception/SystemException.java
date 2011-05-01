package client.exception;

/**
 * The exception thrown when some error in the EMA system occurs.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 27, 2003
 */
public class SystemException extends Exception
{
	public SystemException(String message)
	{
		super(message);
	}
}