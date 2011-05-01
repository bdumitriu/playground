package core;

import core.exceptions.*;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.sql.*;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import client.ClientObserver;

/**
 * An implementation of the Calendar interface.
 *
 * Created by IntelliJ IDEA.
 * User: Lau
 * Date: Mar 14, 2005
 * Time: 2:38:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class CalendarImpl extends UnicastRemoteObject implements Calendar, DomainObject
{
	public CalendarImpl() throws RemoteException
	{
		super();
		observerCount = 0;
		observers = new HashMap<Integer, ClientObserver>();
	}

	public String getUserName()
	{
		return userName;
	}

	public void setUserName(String userName)
	{
		this.userName = userName;
	}

	/**
	 * See the documentation for {@link Calendar#addAppointment(Appointment)}.
	 */
	public int addAppointment(Appointment appointment) throws RemoteException
	{
		appointment.setGroupAppointment(false);
		dbInsertAppointment(appointment);
		return appointment.getId();
	}

	/**
	 * See the documentation for {@link Calendar#changeAppointment(Appointment)}.
	 */
	public void changeAppointment(Appointment appointment) throws RemoteException, InvalidAppointmentIdException
	{
		dbUpdateAppointment(appointment);
	}

	/**
	 * See the documentation for {@link Calendar#deleteAppointment(int)}.
	 */
	public void deleteAppointment(int appointmentId) throws RemoteException, InvalidAppointmentIdException
	{
		dbDeleteAppointment(appointmentId);
	}

	/**
	 * See the documentation for {@link Calendar#getAllAppointments()}.
	 */
	public List<Appointment> getAllAppointments() throws RemoteException
	{
		return AppointmentFinder.getInstance().findForUser(userName);
	}

	/**
	 * See the documentation for {@link Calendar#getAppointmentsWith(String)}.
	 */
	public List<Appointment> getAppointmentsWith(String userName) throws RemoteException
	{
		return AppointmentFinder.getInstance().findAppointmentsWith(userName, this.userName);
	}

	/**
	 * See the documentation for {@link Calendar#getAppointmentsForPeriod(Period)}.
	 */
	public List<Appointment> getAppointmentsForPeriod(Period period) throws RemoteException
	{
		return AppointmentFinder.getInstance().findAppointmentsForPeriod(userName, period);
	}

	/**
	 * See the documentation for {@link Calendar#registerObserver(client.ClientObserver)}.
	 */
	public int registerObserver(ClientObserver observer) throws RemoteException
	{
		int count;
		synchronized (observers)
		{
			observerCount++;
			count = observerCount;
		}
		observers.put(count, observer);

		System.out.println("Registered observer " + count + " for " + userName + "'s calendar.");

		return count;
	}

	/**
	 * See the documentation for {@link Calendar#unregisterObserver(int)}.
	 */
	public void unregisterObserver(int observerId) throws RemoteException
	{
		observers.remove(observerId);

		System.out.println("Unregistered observer " + observerId + " for " + userName + "'s calendar.");
	}

	/**
	 * Notifies all the observers of this calendar that the calendar has changed.
	 */
	public void notifyObservers(int id)
	{
		for (ClientObserver obs : observers.values())
		{
			System.out.println("Notifying observer " + observerCount + " for " + userName + "'s calendar.");
			new NotifierThread(obs, id).start();
		}
	}

	/**
	 * Inserts the <code>appointment</code> into the database.
	 *
	 * @param appointment the appointment that you want to insert into the database
	 */
	private void dbInsertAppointment(Appointment appointment)
	{
		assert userName != null : "The user name is supposed to be set.";
		assert !(userName.equals("")) : "The user name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			//get appointment id from database
			appointment.setIdFromDb(conn);

			//insert the appointment into the Appointments table
			appointment.dbInsert(conn);

			//insert the appointment into the Calendars table
			dbInsert(conn, appointment.getId());

			conn.close();
		}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());
		}
	}

	/**
	 * Updates the <code>appointment</code> in the database.
	 *
	 * @param appointment the appointment that you want to insert into the database
	 * @throws InvalidAppointmentIdException if the apppointment's id does not identify a valid appointment in the
	 *	database.
	 */
	private void dbUpdateAppointment(Appointment appointment) throws InvalidAppointmentIdException
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			appointment.dbUpdate(conn);
			if (appointment.isGroupAppointment())
			{
				List<String> groupUsers = GroupImpl.getGroupForAppointment(appointment.getId()).getGroupUsers();
				CalendarFinder cf = CalendarFinder.getInstance();
				for (int i = 0; i < groupUsers.size(); i++)
				{
					cf.find(groupUsers.get(i)).notifyObservers(appointment.getId()*10+1);
				}
			}
			else
			{
				notifyObservers(appointment.getId()*10+1);
			}
		}
		catch (EmptyGroupException e)
		{}
		catch (NotAGroupAppointmentException e)
		{}
		catch (RemoteException e)
		{}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());
		}
		catch (InvalidAppointmentIdException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			throw e;
		}
	}

	/**
	 * Deletes the appointment identified by <code>appointmentId</code> from the database.
	 *
	 * @param appointmentId the appointmentId of the appointment that you want to delete from the database.
	 * @throws InvalidAppointmentIdException if the apppointment's id does not identify a valid appointment in the
	 *	database.
	 */
	private void dbDeleteAppointment(int appointmentId) throws InvalidAppointmentIdException
	{
		assert userName != null : "The user name is supposed to be set.";
		assert !(userName.equals("")) : "The user name is supposed to be set.";

		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			//delete the appointment from the Calendars table
			dbDelete(conn, appointmentId);

			//delete the appointment from the Appointments table
			Appointment appointment = AppointmentFinder.getInstance().find(appointmentId + "");
			if (appointment == null)
			{
				try
				{
					conn.close();
				}
				catch (SQLException e)
				{
					System.out.println("An SQL exceptions occured: " + e.getMessage());
				}
				finally
				{
					throw new InvalidAppointmentIdException();
				}
			}
			else
			{
				appointment.dbDelete(conn);
			}

			conn.close();
		}
		catch (SQLException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			System.out.println("An SQL exceptions occured: " + e.getMessage());
		}
		catch (InvalidAppointmentIdException e)
		{
			try
			{
				conn.close();
			}
			catch (SQLException se)
			{
				System.out.println("An SQL exceptions occured: " + se.getMessage());
			}

			throw e;
		}
	}

	/**
	 * Inserts an entry into the Calendars table that associates the <code>appointmentId</code> with the user which
	 * owns this calendar.
	 *
	 * @param conn a valid connection to the database.
	 * @param appointmentId the id of the appointment.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 */
	public void dbInsert(Connection conn, int appointmentId) throws SQLException
	{
		PreparedStatement insertStatement = conn.prepareStatement(insertCalendarEntryStatement());
		insertStatement.setString(1, userName);
		insertStatement.setInt(2, appointmentId);

		int rowCount = insertStatement.executeUpdate();
		assert rowCount == 1 : "the row count was not 1 after inserting into the Calendars table";

		if (rowCount == 1)
		{
			notifyObservers(appointmentId*10);
		}
	}

	/**
	 * Deletes the entry from the Calendars table that associates the <code>appointmentId</code> with the user which
	 * owns this calendar.
	 *
	 * @param conn a valid connection to the database.
	 * @param appointmentId the id of the appointment.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 */
	public void dbDelete(Connection conn, int appointmentId) throws SQLException
	{
		PreparedStatement deleteStatement = conn.prepareStatement(deleteCalendarEntryStatement());
		deleteStatement.setString(1, userName);
		deleteStatement.setInt(2, appointmentId);
		int rowCount = deleteStatement.executeUpdate();
		assert rowCount == 1: "the row count was not 1 after deleting from the Calendars table";

		if (rowCount == 1)
		{
			notifyObservers(appointmentId*10+2);
		}
	}

	/*
	private triggerNotify(){
	} */

	private static String insertCalendarEntryStatement()
	{
		return "INSERT INTO Calendars" +
			" VALUES (?, ?)";
	}

	private static String deleteCalendarEntryStatement()
	{
		return "DELETE FROM Calendars" +
			" WHERE userName = ? AND appointmentId = ?";
	}

	private String userName;

	private Map<Integer, ClientObserver> observers;

	private int observerCount;
}

class NotifierThread extends Thread
{
	public NotifierThread(ClientObserver obs, int id)
	{
		this.obs = obs;
		this.id = id;
	}

	public void run()
	{
		try
		{
			obs.notifyObserver(id);
		}
		catch (RemoteException e)
		{
			System.out.println("Got exception when notifying observer: " + e.getMessage());
		}
	}

	private ClientObserver obs;
	private int id;
}