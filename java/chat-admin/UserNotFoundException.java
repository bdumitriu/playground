package chatAdmin;

import java.io.Serializable;

public class UserNotFoundException extends Exception implements Serializable
{
	private String message;

	public UserNotFoundException(String message)
	{
		this.message = message;
	}

	public UserNotFoundException()
	{
		this("The specified user could not be found.");
	}

	public String getMessage()
	{
		return message;
	}
}
