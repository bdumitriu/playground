package core.exceptions;

import java.io.Serializable;

/**
 * This exceptions indicates that the chosen login name is already used by some other user.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 1, 2005
 *
 */
public class DuplicateLoginNameException extends Exception implements Serializable
{
	public DuplicateLoginNameException()
	{
		super("A user with that login name already exists. Please choose a different one.");
	}
}
