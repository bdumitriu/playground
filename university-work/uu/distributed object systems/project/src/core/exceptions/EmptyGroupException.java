package core.exceptions;

import java.io.Serializable;

/**
 * This exception indicates that some operation was tried on an empty group.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 2, 2005
 */
public class EmptyGroupException extends Exception implements Serializable
{
	public EmptyGroupException()
	{
		super("There is no user in the group.");
	}
}
