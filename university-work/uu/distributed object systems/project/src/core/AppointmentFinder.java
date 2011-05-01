package core;

import static java.lang.Integer.parseInt;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.ArrayList;
import java.util.Date;

/**
 * A singleton class which provides some means of retrieving appointments from the database as Appointment objects.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 10, 2005
 */
public class AppointmentFinder extends AbstractFinder
{
	public static AppointmentFinder getInstance()
	{
		return instance;
	}

	/**
	 * Finds and returns an appointment identified by <code>id</code>.
	 *
	 * @param id the id of the appointment you want to find.
	 * @return the appointment identified by <code>id</code>.
	 */
	public Appointment find(String id)
	{
		return (Appointment) abstractFind(id);
	}

	/**
	 * Finds and returns all the appointments that belong to the user identified by <code>loginName</code>.
	 *
	 * @param loginName the login name of the user whose appointments you want to retrieve.
	 * @return a list with all the appointments that belong to the user identified by <code>loginName</code>.
	 */
	public List<Appointment> findForUser(String loginName)
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			PreparedStatement findStatement = conn.prepareStatement(findForUserStatement(),
				ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY);
			findStatement.setString(1, loginName);
			return genericFindAppointments(conn, findStatement);
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

			return null;
		}
	}

	/**
	 * Finds and returns all the appointments that the user identified by <code>loginName</code> has with the user
	 * identified by <code>name</code>.
	 *
	 * @param name the login name of the user who should be part of the appointments you want to retrieve.
	 * @param loginName the login name of the user whose appointments you want to retrieve.
	 * @return a list with all the appointments that the user identified by <code>loginName</code> has with the user
	 *	identified by <code>name</code>.
	 */
	public List<Appointment> findAppointmentsWith(String name, String loginName)
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			PreparedStatement findStatement = conn.prepareStatement(findWithStatement(),
				ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY);
			findStatement.setString(1, loginName);
			findStatement.setString(2, name);
			return genericFindAppointments(conn, findStatement);
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

			return null;
		}
	}

	/**
	 * Finds and returns all the appointments that belong to the user identified by <code>loginName</code> and which
	 * are in the specified <code>period</code>.
	 *
	 * @param loginName the login name of the user whose appointments you want to retrieve.
	 * @param period the period in which the appointments you want should be.
	 * @return a list with all the appointments that belong to the user identified by <code>loginName</code> and
	 *	which are in the specified <code>period</code>.
	 */
	public List<Appointment> findAppointmentsForPeriod(String loginName, Period period)
	{
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			PreparedStatement findStatement = conn.prepareStatement(findForPeriodStatement(),
				ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY);
			findStatement.setString(1, loginName);
			findStatement.setTimestamp(2, new java.sql.Timestamp(period.getStartDate().getTime()));
			findStatement.setTimestamp(3, new java.sql.Timestamp(period.getEndDate().getTime()));
			return genericFindAppointments(conn, findStatement);
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

			return null;
		}
	}

	private List<Appointment> genericFindAppointments(Connection conn, PreparedStatement findStatement)
	{
		try
		{
			ResultSet rs = findStatement.executeQuery();

			int nrRows = 0;
			if (rs.last())
			{
				nrRows = rs.getRow();
			}
			rs.beforeFirst();

			ArrayList<Appointment> apps = new ArrayList<Appointment>(nrRows);
			while (rs.next())
			{
				apps.add((Appointment) load(rs));
			}

			conn.close();

			return apps;
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

			return null;
		}
	}

	protected DomainObject doLoad(String id, ResultSet rs) throws SQLException
	{
		Appointment appointment = new Appointment();
		appointment.setId(parseInt(id));
		appointment.setTimeSlot(new Period(new Date(rs.getTimestamp(2).getTime()),
			new Date(rs.getTimestamp(3).getTime())));
		appointment.setDescription(rs.getString(4));
		appointment.setLocation(rs.getString(5));
		appointment.setGroupAppointment(rs.getBoolean(6));

		return appointment;
	}

	protected String findStatement()
	{
		return "SELECT id, starttime, endtime, description, location, isgroupapp" +
			" FROM Appointments" +
			" WHERE id = ?";
	}

	private AppointmentFinder()
	{
		super();
	}

	private static String findForUserStatement()
	{
		return "SELECT id, starttime, endtime, description, location, isgroupapp" +
			" FROM Appointments, Calendars" +
			" WHERE userName = ? AND appointmentid = id";
	}

	private static String findWithStatement()
	{
		return "SELECT id, starttime, endtime, description, location, isgroupapp" +
			" FROM Appointments, Calendars" +
			" WHERE userName = ? AND appointmentid = id AND appointmentid IN (SELECT appointmentid FROM Calendars WHERE userName = ?)";
	}

	private static String findForPeriodStatement()
	{
		return "SELECT id, starttime, endtime, description, location, isgroupapp" +
			" FROM Appointments, Calendars" +
			" WHERE userName = ? AND appointmentid = id AND starttime >= ? AND endtime <= ?";
	}

	private static AppointmentFinder instance = new AppointmentFinder();
}
