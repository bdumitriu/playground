package chatAdmin;

import java.io.Serializable;

public class LockException extends Exception implements Serializable
{
	public LockException()
	{}

	public String getMessage()
	{
		return "The server has been locked by the 'admin' superuser." +
			" Please try again later.";
	}
}
