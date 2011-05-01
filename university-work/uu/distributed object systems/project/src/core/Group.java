package core;

import core.exceptions.*;

import java.util.Date;
import java.util.List;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.io.Serializable;

/**
 * This interface defines the methods that have to be implemented in order to support the group appointments
 * functionality of the system.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 2, 2005
 */
public interface Group extends Remote
{
	/**
	 * Returns a list of the time slots which could be used by this group of users for scheduling a meeting. You
	 * should always specifiy an earliest and latest date. Some default values are used if either of them is null,
	 * but they most probably aren't what you want. So set them!
	 *
	 * @param duration the duration of the meeting (in minutes).
	 * @param earliest the earliest date when the meeting can be scheduled.
	 * @param latest the latest date when then meeting can be scheduled.
	 * @param granularity the granularity of the suggestions.
	 * @return a (possibly empty) list of possible time slots.
	 * @throws EmptyGroupException if there are no users in the group.
	 */
	public List<Period> getTimeSlots(int duration, Date earliest, Date latest, Granularity granularity)
		throws EmptyGroupException, RemoteException;

	/**
	 * Creates a group <code>appointment</code> which includes all the users in this group. You should be aware that
	 * it is possible for this method to fail, even if you correctly chose one of the time slots returned by
	 * {@link #getTimeSlots(int, java.util.Date, java.util.Date, Granularity)}, since in the time between the two
	 * calls, one of the users of the group might have created an appointment which spans over a part of or the
	 * entire time slot which you specify for the group appointment.
	 *
	 * @param appointment the appointment to create.
	 * @return the id of the created appointment.
	 * @throws EmptyGroupException if there are no users in the group.
	 * @throws TimeSlotNotAvailableException if the time slot set for the <code>appointment</code> is not available
	 *	for all the users.
	 */
	public int createGroupAppointment(Appointment appointment)
		throws EmptyGroupException, TimeSlotNotAvailableException, RemoteException;

	/**
	 * Moves the group appointment specified by <code>appointmentId</code> to the <code>newTimeSlot</code>. Same
	 * comment about the possibility of failure as for {@link #createGroupAppointment(Appointment)}.
	 *
	 * @param appointmentId the id which identifies the appointment to move.
	 * @param newTimeSlot the new time slot of the appointment.
	 * @throws EmptyGroupException if there are no users in the group.
	 * @throws TimeSlotNotAvailableException if the new time slot set for the <code>appointment</code> is not
	 *	available for all the users.
	 * @throws InvalidAppointmentIdException if the <code>appointmentId</code> does not identify a valid
	 *	appointment.
	 */
	public void moveGroupAppointment(int appointmentId, Period newTimeSlot)
		throws EmptyGroupException, TimeSlotNotAvailableException, InvalidAppointmentIdException, RemoteException;

	/**
	 * Deletes the group appointment specified by <code>appointmentId</code>.
	 *
	 * @param appointmentId the id which identifies the appointment to delete.
	 * @throws EmptyGroupException if there are no users in the group.
	 * @throws InvalidAppointmentIdException if the <code>appointmentId</code> does not identify a valid
	 *	appointment.
	 */
	public void deleteGroupAppoiment(int appointmentId)
		throws EmptyGroupException, InvalidAppointmentIdException, RemoteException;

	/**
	 * Returns the list of users in the group.
	 *
	 * @return the list of users in the group.
	 */
	public List<String> getGroupUsers()
		throws RemoteException;
}
