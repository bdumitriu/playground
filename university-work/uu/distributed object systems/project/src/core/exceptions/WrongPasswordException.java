package core.exceptions;

import java.io.Serializable;

/**
 * This exceptions indicates that the specified password was incorrect.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 1, 2005
 */
public class WrongPasswordException extends Exception implements Serializable
{
	public WrongPasswordException()
	{
		super("The password is not correct.");
	}
}
