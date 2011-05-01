
/**
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Jul 31, 2004
 */
public class InvalidStateException extends Exception
{
	public InvalidStateException(String message)
	{
		this.message = message;
	}

	public String getMessage()
	{
		return message;
	}

	private String message;
}
