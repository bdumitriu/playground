package core.exceptions;

import java.io.Serializable;

/**
 * This exceptions indicates that the specified appointment id was invalid in a certain context.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 1, 2005
 */
public class InvalidAppointmentIdException extends Exception implements Serializable
{
	public InvalidAppointmentIdException()
	{
		super("The id of the appointment was invalid.");
	}
}
