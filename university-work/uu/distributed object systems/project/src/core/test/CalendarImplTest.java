package core.test;

import core.*;
import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;
import junit.framework.TestCase;

import java.util.Date;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.sql.Statement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Connection;

/**
 * Created by IntelliJ IDEA.
 * User: Lau
 * Date: Mar 15, 2005
 * Time: 12:33:53 AM
 * To change this template use File | Settings | File Templates.
 */
public class CalendarImplTest extends TestCase
{

	protected void setUp() throws Exception
	{
		appFinder = AppointmentFinder.getInstance();
		calendar = new CalendarImpl();
		calendar.setUserName("bogdan");
	}

	public void testInsertAppointment() throws Exception
	{
		Appointment app = createAppointment1();
		try
		{
			calendar.addAppointment(app);
			calendar.deleteAppointment(app.getId());
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	public void testChangeAppointment() throws Exception
	{
		Appointment app = createAppointment1();

		try
		{
			//insert app
			calendar.addAppointment(app);
			//change some stuff
			String newDescription = "zip zap";
			app.setDescription(newDescription);
			String newLocation = "Cambridgelaan ???";
			app.setLocation(newLocation);
			//write it to db
			calendar.changeAppointment(app);
			//check values
			Appointment appCheck = appFinder.find(app.getId() + "");
			Assert.assertEquals(appCheck.getDescription(), newDescription);
			Assert.assertEquals(appCheck.getLocation(), newLocation);
			//remove app
			calendar.deleteAppointment(app.getId());
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	//runs under the assumption that there are only three appointments in Bogdan's calendar with id 1,2,3
	public void testGetAllAppointments() throws Exception
	{
		List appz = calendar.getAllAppointments();
		Iterator it = appz.iterator();
		for (int i = 1; it.hasNext(); i++)
		{
			Appointment app = (Appointment) it.next();
			Assert.assertEquals(app.getId(), i);
		}
	}

	public void testGetAppointmentsWith() throws Exception
	{
		List appz = calendar.getAppointmentsWith("t2");
		Iterator it = appz.iterator();
		for (int i = 1; it.hasNext(); i++)
		{
			Appointment app = (Appointment) it.next();
			System.out.println(app.toString());
		}
	}

	private int getNextID()
	{
		int appointmentID = 0;
		Connection conn = ConnectionManager.getInstance().getConnection();
		try
		{
			Statement stmt = conn.createStatement();
			String query = "select nextval('Appointments_id_seq')";
			ResultSet rs = stmt.executeQuery(query);
			if (rs.next())
			{
				appointmentID = rs.getInt(1);
			}
			return appointmentID;
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
				System.exit(1);
				return appointmentID;
			}
			System.out.println("An SQL exceptions occured: " + e.getMessage());
			System.exit(1);
			return appointmentID;
		}
	}

	private Appointment createAppointment1()
	{
		Appointment app = new Appointment();
		Period period = new Period();
		java.util.Calendar cal = java.util.Calendar.getInstance();
		period.setStartDate(new Date(cal.getTimeInMillis()));
		period.setEndDate(new Date(cal.getTimeInMillis()));
		app.setTimeSlot(period);
		app.setLocation("Utrecht");
		app.setDescription("blabla");
		return app;
	}

	public static Test suite()
	{
		return new TestSuite(CalendarImplTest.class);
	}

	AppointmentFinder appFinder;
	CalendarImpl calendar;
}
