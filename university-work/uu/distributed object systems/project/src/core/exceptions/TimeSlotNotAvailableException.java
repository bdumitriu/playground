package core.exceptions;

import java.io.Serializable;

/**
 * This exception indicates an attempt to create an appointment with a time slot which overlaps on already existing
 * appointments. 
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 2, 2005
 */
public class TimeSlotNotAvailableException extends Exception implements Serializable
{
	public TimeSlotNotAvailableException()
	{
		super("The time slot you have selected for the appointment is not available.");
	}
}
