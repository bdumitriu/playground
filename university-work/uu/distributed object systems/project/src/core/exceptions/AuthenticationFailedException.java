package core.exceptions;

import java.io.Serializable;

/**
 * This exception indicates that the supplied authentication token was not valid.
 * 
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 2, 2005
 */
public class AuthenticationFailedException extends Exception implements Serializable
{
	public AuthenticationFailedException()
	{
		super("The authentication protocol has failed. You probably have to log in again.");
	}
}
