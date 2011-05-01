package core;

import core.exceptions.InvalidAppointmentIdException;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Date;
import java.io.Serializable;

import client.ClientObserver;

/**
 * This interface lists all the methods which have to be implemented in order to support the calendar functionality of
 * the system.
 *
 * Created by IntelliJ IDEA.
 * User: Lau
 * Date: Mar 14, 2005
 * Time: 2:40:20 PM
 * To change this template use File | Settings | File Templates.
 */
public interface Calendar extends Remote
{
	/**
	 * Adds the <code>appointment</code> to the calendar. You don't have to set the id of the appointment, as this
	 * will be set automatically anyway (and returned as the result of this method).
	 *
	 * @param appointment the appointment that you want to add to the calendar.
	 * @return the id of the added appointment.
	 */
	public int addAppointment(Appointment appointment) throws RemoteException;

	/**
	 * Changes the <code>appointment</code> in the calendar. You are supposed to set the id of the appointment to
	 * the id of the appointment you want to change and <b>all</b> the details of the appointment to their new
	 * values, since <b>all</b> of them will be written to the database.
	 *
	 * @param appointment the appointment that you want to change in the calendar.
	 * @throws InvalidAppointmentIdException if the <code>apppointment</code>'s id does not identify a valid
	 *	appointment.
	 */
	public void changeAppointment(Appointment appointment) throws RemoteException, InvalidAppointmentIdException;

	/**
	 * Deletes the appointment identified by <code>appointmentId</code> from the calendar.
	 *
	 * @param appointmentId the appointmentId of the appointment that you want to delete from the calendar.
	 * @throws InvalidAppointmentIdException if the <code>apppointmentId</code> does not identify a valid
	 *	appointment.
	 */
	public void deleteAppointment(int appointmentId) throws RemoteException, InvalidAppointmentIdException;

	/**
	 * Returns all the appointments from this calendar.
	 *
	 * @return a list of appointments that are in this calendar.
	 */
	public List<Appointment> getAllAppointments() throws RemoteException;

	/**
	 * Returns all the appointments with the user identified by <code>userName<code>.
	 *
	 * @param userName the login name of the the user that you want to view the shared appointments with.
	 * @return a list of appointments that you have with the user.
	 */
	public List<Appointment> getAppointmentsWith(String userName) throws RemoteException;

	/**
	 * Returns a list of the appointments which are scheduled in the specified date.
	 *
	 * @param day the day part of the date.
	 * @param month the month part of the date.
	 * @param year the year part of the date.
	 * @return a list of appointments in a certain day.
	 */
	//public List<Appointment> getAppointmentForDay(int day, int month, int year) throws RemoteException;

	/**
	 * Returns a list of the appointments which are scheduled in the week number <code>weekNumber</code> of
	 * the year <code>year</code>.
	 *
	 * @param weekNumber the number of the week for which you want the appointments.
	 * @param year the year for which you want the appointments.
	 * @return a list of appointments in a certain week of a certain year.
	 */
	//public List<Appointment> getAppointmentsForWeek(int weekNumber, int year) throws RemoteException;

	/**
	 * Returns a list of the appointments which are scheduled in the month <code>monthNumber</code> of
	 * the year <code>year</code>.
	 *
	 * @param monthNumber the number of the month (1 for January, 2 for February and so on) for which you want the
	 *	appointments.
	 * @param year the year for which you want the appointments.
	 * @return a list of appointments in a certain month of a certain year.
	 */
	//public List<Appointment> getAppointmentsForMonth(int monthNumber, int year) throws RemoteException;

	/**
	 * Returns a list of the appointments which are scheduled in the period <code>period</code>.
	 *
	 * @param period the period for which you want the appointments.
	 * @return a list of appointments in a certain period.
	 */
	public List<Appointment> getAppointmentsForPeriod(Period period) throws RemoteException;

	/**
	 * Returns the first appointment that comes after the specified <code>date</code>.
	 *
	 * @return the first appointment that comes after the specified <code>date</code>.
	 */
	//public Appointment getFirstAppointmentAfter(Date date) throws RemoteException;

	/**
	 * Registers an observer with this calender, observer which will be notified whenever the calendar changes.
	 *
	 * @return the id which you can use to unregister this observer.
	 */
	public int registerObserver(ClientObserver observer) throws RemoteException;

	/**
	 * Unregisters the observer identified by <code>observerId</code>.
	 *
	 * @param observerId the id of the observer to unregister.
	 */
	public void unregisterObserver(int observerId) throws RemoteException;
}
