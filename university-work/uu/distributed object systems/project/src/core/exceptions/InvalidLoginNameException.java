package core.exceptions;

import java.io.Serializable;

/**
 * This exceptions indicates that the specified login name was invalid in a certain context.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 1, 2005
 */
public class InvalidLoginNameException extends Exception implements Serializable
{
	public InvalidLoginNameException()
	{
		super("The login name you have supplied was not valid.");
	}
}
