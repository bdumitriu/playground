package core;

import static java.util.GregorianCalendar.*;

import core.exceptions.*;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.util.*;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

/**
 * An implementation of the Group interface.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 2, 2005
 */
public class GroupImpl extends UnicastRemoteObject implements Group
{
	/**
	 * Creates a new group with the list of <code>users</code> as its members.
	 *
	 * @param users the list of login names of the users of the group.
	 */
	public GroupImpl(List<String> users) throws RemoteException
	{
		super();
		groupUsers = users;
		if (groupUsers == null)
		{
			groupUsers = new ArrayList<String>();
		}
	}

	/**
	 * Creates an instance of GroupImpl with the users that share the group appointment identified by
	 * <code>appointmentId</code>.
	 *
	 * @param appointmentId the id of the group appointment.
	 * @return a new instance of GroupImpl.
	 * @throws EmptyGroupException if the group contains no members.
	 * @throws NotAGroupAppointmentException if the appointment id was not that of a group appointment.
	 */
	public static Group getGroupForAppointment(int appointmentId)
		throws EmptyGroupException, NotAGroupAppointmentException
	{
		Appointment app = AppointmentFinder.getInstance().find(appointmentId + "");
		if (!app.isGroupAppointment())
		{
			throw new NotAGroupAppointmentException();
		}

		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			PreparedStatement prepStatement = conn.prepareStatement(getUsersStatement());
			prepStatement.setInt(1, appointmentId);
			ResultSet rs = prepStatement.executeQuery();

			ArrayList<String> users = new ArrayList<String>();
			while (rs.next())
			{
				users.add(rs.getString("userName"));
			}

			conn.close();

			if (users.size() == 0)
			{
				throw new EmptyGroupException();
			}
			else
			{
				try
				{
					return new GroupImpl(users);
				}
				catch (RemoteException e)
				{
					// it will never be thrown since this is a local call
				}
			}
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

		throw new EmptyGroupException();
	}

	public List<String> getGroupUsers()
		throws RemoteException
	{
		return groupUsers;
	}

	/**
	 * See the documentation for {@link Group#getTimeSlots(int, java.util.Date, java.util.Date, Granularity)}.
	 */
	public List<Period> getTimeSlots(int duration, Date earliest, Date latest, Granularity granularity)
		throws EmptyGroupException, RemoteException
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		List<Period> result = null;
		try
		{
			PreparedStatement prepStm = conn.prepareStatement(groupAppointmentsStatement());
			ResultSet apps = prepStm.executeQuery();

			// put the data in an easier to use structure
			HashMap<String, ArrayList<Period>> userApps = extractAppointmentData(apps);

			// compute the available time slots
			result = computeTimeSlots(userApps, duration, earliest, latest, granularity);

			conn.close();
		}
		catch (EmptyGroupException e)
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

		return result;
	}

	/**
	 * See the documentation for {@link Group#createGroupAppointment(Appointment)}.
	 */
	public int createGroupAppointment(Appointment appointment)
		throws EmptyGroupException, TimeSlotNotAvailableException, RemoteException
	{
		if (groupUsers.size() == 0)
		{
			throw new EmptyGroupException();
		}

		Connection conn = ConnectionManager.getInstance().getConnection();

		try
		{
			conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
			conn.setAutoCommit(false);

			PreparedStatement prepStm = conn.prepareStatement(groupAppointmentsStatement());

			ResultSet apps = prepStm.executeQuery();

			// put the data in an easier to use structure
			HashMap<String, ArrayList<Period>> userApps = extractAppointmentData(apps);

			if (checkTimeSlot(userApps, appointment.getTimeSlot()))
			{
				// get appointment id from database
				appointment.setIdFromDb(conn);

				appointment.setGroupAppointment(true);

				// insert the appointment into the Appointments table
				appointment.dbInsert(conn);

				// insert the appointment into the calendar of each user
				CalendarFinder cf = CalendarFinder.getInstance();
				int appId = appointment.getId();
				for (int i = 0; i < groupUsers.size(); i++)
				{
					cf.find(groupUsers.get(i)).dbInsert(conn, appId);
				}

				conn.commit();
				conn.close();
			}
			else
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
					throw new TimeSlotNotAvailableException();
				}
			}
		}
		catch (EmptyGroupException e)
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

		return appointment.getId();
	}

	/**
	 * See the documentation for {@link Group#moveGroupAppointment(int, Period)}.
	 */
	public void moveGroupAppointment(int appointmentId, Period newTimeSlot)
		throws EmptyGroupException, TimeSlotNotAvailableException, InvalidAppointmentIdException, RemoteException
	{
		if (groupUsers.size() == 0)
		{
			throw new EmptyGroupException();
		}

		Connection conn = ConnectionManager.getInstance().getConnection();

		try
		{
			conn.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
			conn.setAutoCommit(false);

			PreparedStatement prepStm = conn.prepareStatement(groupAppointmentsStatement());

			ResultSet apps = prepStm.executeQuery();

			// put the data in an easier to use structure
			HashMap<String, ArrayList<Period>> userApps = extractAppointmentData(apps);

			if (checkTimeSlot(userApps, newTimeSlot))
			{
				Appointment appointment = AppointmentFinder.getInstance().find(appointmentId + "");
				if (appointment == null)
				{
					throw new InvalidAppointmentIdException();
				}

				appointment.setTimeSlot(newTimeSlot);
				appointment.setGroupAppointment(true);
				appointment.dbUpdate(conn);

				conn.commit();
				conn.close();

				CalendarFinder cf = CalendarFinder.getInstance();
				for (int i = 0; i < groupUsers.size(); i++)
				{
					cf.find(groupUsers.get(i)).notifyObservers(appointmentId*10+1);
				}
			}
			else
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
					throw new TimeSlotNotAvailableException();
				}
			}
		}
		catch (EmptyGroupException e)
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
	 * See the documentation for {@link Group#deleteGroupAppoiment(int)}.
	 */
	public void deleteGroupAppoiment(int appointmentId)
		throws EmptyGroupException, InvalidAppointmentIdException, RemoteException
	{
		if (groupUsers.size() == 0)
		{
			throw new EmptyGroupException();
		}

		Connection conn = ConnectionManager.getInstance().getConnection();

		try
		{
			// delete the appointment from the calendar of each user
			CalendarFinder cf = CalendarFinder.getInstance();
			for (int i = 0; i < groupUsers.size(); i++)
			{
				cf.find(groupUsers.get(i)).dbDelete(conn, appointmentId);
			}

			Appointment appointment = AppointmentFinder.getInstance().find(appointmentId + "");
			if (appointment == null)
			{
				throw new InvalidAppointmentIdException();
			}

			appointment.dbDelete(conn);

			conn.close();
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
	 * Takes a ResultSet returned by a statement that uses {@link #groupAppointmentsStatement()} and puts the data
	 * therin in a HashMap which uses the user names as keys and lists of {@link Period}'s as values. Each entry
	 * associates a user name with his appointments.
	 *
	 * @param apps the ResultSet containing the data.
	 * @return the constructed {@link java.util.HashMap}.
	 * @throws SQLException if an SQLException occurs while communicating to the database, it is forwarded to the
	 *	caller.
	 */
	private HashMap<String, ArrayList<Period>> extractAppointmentData(ResultSet apps)
		throws SQLException
	{
		// create & initialize a structure for putting the necessary data into
		HashMap<String, ArrayList<Period>> userApps = new HashMap<String, ArrayList<Period>>();
		for (int i = 0; i < groupUsers.size(); i++)
		{
			userApps.put(groupUsers.get(i), new ArrayList<Period>());
		}

		// put all the appointments of the group users into the structure
		while (apps.next())
		{
			String userName = apps.getString("userName");
			Date startTime = new Date(apps.getTimestamp("startTime").getTime());
			Date endTime = new Date(apps.getTimestamp("endTime").getTime());
			ArrayList<Period> userList = userApps.get(userName);
			if (userList != null)
			{
				userList.add(new Period(startTime, endTime));
			}
		}

		return userApps;
	}

	/**
	 * Runs an algorithm to compute the time slots which are available for all users and returns a list of these
	 * time slots.
	 *
	 * @param userApps the structure returned by {@link #extractAppointmentData(java.sql.ResultSet)}.
	 * @param duration the duration of the meeting.
	 * @param earliest the earliest date when the meeting can be scheduled.
	 * @param latest the latest date when the meeting can be scheduled.
	 * @param granularity the granularity of the time slots.
	 * @return the list of time slots.
	 */
	private List<Period> computeTimeSlots(HashMap<String, ArrayList<Period>> userApps,
		int duration, Date earliest, Date latest, Granularity granularity)
	{
		if (earliest == null && latest == null)
		{
			earliest = new Date();
			// earliest + 1 week
			latest = new Date(earliest.getTime() + 7 * 24 * 60 * 60 * 1000);
		}
		else if (earliest == null && latest != null)
		{
			// latest - 1 week
			earliest = new Date(latest.getTime() - 7 * 24 * 60 * 60 * 1000);
		}
		else if (earliest != null && latest == null)
		{
			// earliest + 1 week
			latest = new Date(earliest.getTime() + 7 * 24 * 60 * 60 * 1000);
		}
		else if (earliest.after(latest))
		{
			return new ArrayList<Period>();
		}

		earliest = roundUp(earliest, granularity);
		latest = roundDown(latest, granularity);

		int granMillis = -1;
		switch (granularity)
		{
			case FIFTEEN_MINUTES:
				granMillis = 15 * 60 * 1000;
				break;
			case THIRTY_MINUTES:
				granMillis = 30 * 60 * 1000;
				break;
			case ONE_HOUR:
				granMillis = 60 * 60 * 1000;
				break;
			default:
				granMillis = 30 * 60 * 1000;
				break;
		}

		/*
		 * For each user, we're going to have a BitSet with the following significance: bit n (0-based) of the
		 * BitSet is true if the user has an appointment (or even just a part thereof) in the time slot
		 * which begins at earliest.getTime() + (granMillis * n) and ends at earliest.getTime() +
		 * (granMillis * (n+1)).
		 */

		ArrayList<BitSet> calendars = new ArrayList<BitSet>(userApps.size());

		int nbits = (int) ((latest.getTime() - earliest.getTime()) / granMillis);
		long from = earliest.getTime();
		long to = latest.getTime();
		for (ArrayList<Period> cal : userApps.values())
		{
			BitSet bitSet = new BitSet(nbits);
			for (Period p : cal)
			{
				long startTime = p.getStartDate().getTime();
				long endTime = p.getEndDate().getTime();

				int bitStart = 0;
				if (startTime >= from)
				{
					bitStart = (int) ((startTime - from) / granMillis);
				}

				int bitEnd = nbits - 1;
				if (endTime <= from)
				{
					bitEnd = -1;
				}
				else if (endTime <= to)
				{
					bitEnd = (int) ((endTime - from - 1) / granMillis);
				}

				if (bitStart <= bitEnd)
				{
					bitSet.set(bitStart, bitEnd + 1);
				}
			}

			calendars.add(bitSet);
		}

		BitSet result = new BitSet(nbits);
		for (BitSet bitSet : calendars)
		{
			result.or(bitSet);
		}

		// transform duration from minutes to milliseconds and round it up to a multiple of the granularity
		int durationMillis = duration * 60 * 1000;
		if (durationMillis % granMillis != 0)
		{
			durationMillis += (granMillis - (durationMillis % granMillis));
		}

		int durationBits = (int) (durationMillis / granMillis);

		ArrayList<Period> timeSlots = new ArrayList<Period>();
		for (int i = 0; i < nbits - durationBits + 1; i++)
		{
			if (!result.get(i))
			{
				int nsb = result.nextSetBit(i + 1);

				if (nsb == -1 || nsb >= nbits || (nsb - i) >= durationBits)
				{
					timeSlots.add(new Period(new Date(from + granMillis * i),
						new Date(from + granMillis * (i + durationBits))));
				}
			}
		}

		return timeSlots;
	}

	/**
	 * Checks whether the <code>period</code> is free for all users in the <code>userApps</code> HashMap.
	 *
	 * @param userApps the structure returned by {@link #extractAppointmentData(java.sql.ResultSet)}.
	 * @param period the period to check for availability.
	 * @return true if the period is available, false if it is not.
	 */
	private boolean checkTimeSlot(HashMap<String, ArrayList<Period>> userApps, Period period)
	{
		long startTime = period.getStartDate().getTime();
		long endTime = period.getEndDate().getTime();

		for (ArrayList<Period> cal : userApps.values())
		{
			for (Period p : cal)
			{
				if (startTime > p.getStartDate().getTime() &&
					startTime < p.getEndDate().getTime())
				{
					return false;
				}
				if (endTime > p.getStartDate().getTime() &&
					endTime < p.getEndDate().getTime())
				{
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Rounds the <code>date<code> down to the nearest date which is a multiple of the <code>granularity</code>.
	 *
	 * @return the rounded down date.
	 */
	private Date roundDown(Date date, Granularity granularity)
	{
		GregorianCalendar cal = new GregorianCalendar();
		cal.setTimeInMillis(date.getTime());
		int minutes = cal.get(MINUTE);
		switch (granularity)
		{
			case FIFTEEN_MINUTES:
				cal.add(MINUTE, -(minutes % 15));
				break;
			case THIRTY_MINUTES:
				cal.add(MINUTE, -(minutes % 30));
				break;
			case ONE_HOUR:
				cal.set(MINUTE, 0);
				break;
		}
		cal.set(SECOND, 0);
		cal.set(MILLISECOND, 0);
		return cal.getTime();
	}

	/**
	 * Rounds the <code>date<code> up to the nearest date which is a multiple of the <code>granularity</code>.
	 *
	 * @return the rounded up date.
	 */
	private Date roundUp(Date date, Granularity granularity)
	{
		GregorianCalendar cal = new GregorianCalendar();
		cal.setTimeInMillis(date.getTime());
		int minutes = cal.get(MINUTE);
		switch (granularity)
		{
			case FIFTEEN_MINUTES:
				if (minutes % 15 != 0)
					cal.add(MINUTE, 15 - (minutes % 15));
				break;
			case THIRTY_MINUTES:
				if (minutes % 30 != 0)
					cal.add(MINUTE, 30 - (minutes % 30));
				break;
			case ONE_HOUR:
				if (minutes != 0)
				{
					cal.set(MINUTE, 0);
					cal.add(HOUR_OF_DAY, 1);
				}
				break;
		}
		cal.set(SECOND, 0);
		cal.set(MILLISECOND, 0);
		return cal.getTime();
	}

	private String groupAppointmentsStatement() throws EmptyGroupException
	{
		return "SELECT userName, startTime, endTime" +
			" FROM Calendars, Appointments" +
			" WHERE appointmentId = id AND userName IN " + buildUserList();
	}

	private static String getUsersStatement()
	{
		return "SELECT userName" +
			" FROM Calendars" +
			" WHERE appointmentId = ?";
	}

	private String buildUserList() throws EmptyGroupException
	{
		if (groupUsers.size() == 0)
		{
			throw new EmptyGroupException();
		}

		StringBuilder sb = new StringBuilder("(");
		sb.append("'");
		sb.append(groupUsers.get(0));
		sb.append("'");
		for (int i = 1; i < groupUsers.size(); i++)
		{
			sb.append(", '");
			sb.append(groupUsers.get(i));
			sb.append("'");
		}
		sb.append(")");

		return sb.toString();
	}

	private List<String> groupUsers;
}
