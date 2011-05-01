package chatAdmin;

import java.io.Serializable;

public class InvalidPasswordException extends Exception implements Serializable
{
	private String message;

	public InvalidPasswordException(String message)
	{
		this.message = message;
	}

	public InvalidPasswordException()
	{
		this("The specified password was incorrect.");
	}

	public String getMessage()
	{
		return message;
	}
}
