package core;

import core.exceptions.AuthenticationFailedException;
import core.exceptions.InvalidAppointmentIdException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

/**
 * This interface contains all the methods which need to be implemented for supporting various calendar management
 * functions for which there is no direct entry point.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 8, 2005
 */
public interface CalendarManagement extends Remote
{
	/**
	 * Creates a group consisting of the users with the names in the <code>users</code> list. This group will also
	 * contain the user associated with <code>authToken</code>, should he not already be in the <code>users</code>
	 * list.
	 *
	 * @param authToken a token by which the system can check if the caller is logged in and also identify him.
	 * @param users a list containing the login names of the users you want in the group.
	 * @return the created group.
	 * @throws core.exceptions.AuthenticationFailedException if the supplied <code>authToken</code> is invalid.
	 */
	Group createGroup(String authToken, List<String> users)
		throws RemoteException, AuthenticationFailedException;

	/**
	 * Creates an instance of Group with the users that share the group appointment identified by
	 * <code>appointmentId</code>. Retrieving this instance is only allowed if the user associated with
	 * <code>authToken</code> is also part of this group. Otherwise, null is returned.
	 *
	 * @param authToken a token by which the system can check if the caller is logged in and also identify him.
	 * @param appointmentId the id of the group appointment.
	 * @return a Group with the users that are involved in the appointment.
	 * @throws core.exceptions.InvalidAppointmentIdException if the supplied <code>appointmentId</code> either
	 *	<ul>
	 *	<li>does not identify a valid appointment</li>
	 *	<li>identifies an appointment which is not a group appointment</li>
	 *	<li>identifies an appointment which is shared by an empty group</li>
	 *	<li>identifies an appointment which is shared by a group that does not contain the user associated with
	 *	<code>authToken</code>.</li>
	 *	</ul>
	 * @throws core.exceptions.AuthenticationFailedException if the supplied <code>authToken</code> is invalid.
	 */
	Group getGroupForAppointment(String authToken, int appointmentId)
		throws RemoteException, InvalidAppointmentIdException, AuthenticationFailedException;
}
