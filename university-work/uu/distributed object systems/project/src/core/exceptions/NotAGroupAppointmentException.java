package core.exceptions;

import java.io.Serializable;

/**
 * This exceptions indicates that the appointment you tried to get a group for was not a group appointment.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 9, 2005
 */
public class NotAGroupAppointmentException extends Exception implements Serializable
{
	public NotAGroupAppointmentException()
	{
		super("The appointment was not a group appointment.");
	}
}
