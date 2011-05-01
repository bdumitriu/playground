package core.test;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 12, 2005
 */

import static java.util.GregorianCalendar.*;
import junit.framework.*;
import core.*;
import core.exceptions.*;
import core.exceptions.TimeSlotNotAvailableException;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.sql.Connection;

public class GroupImplTest extends TestCase
{
	protected void setUp() throws Exception
	{
		ArrayList<String> userNames = new ArrayList<String>();
		userNames.add("user1");
		userNames.add("user2");
		userNames.add("user3");

		group = new GroupImpl(userNames);

		user1 = new UserImpl();
		user1.setLoginName("user1");
		user1.dbInsert("");
		user2 = new UserImpl();
		user2.setLoginName("user2");
		user2.dbInsert("");
		user3 = new UserImpl();
		user3.setLoginName("user3");
		user3.dbInsert("");

		CalendarImpl cal1 = new CalendarImpl();
		cal1.setUserName("user1");
		user1.setCalendar(cal1);
		CalendarImpl cal2 = new CalendarImpl();
		cal2.setUserName("user2");
		user2.setCalendar(cal2);
		CalendarImpl cal3 = new CalendarImpl();
		cal3.setUserName("user3");
		user3.setCalendar(cal3);

		Appointment app;
		appIds = new ArrayList<Integer>();

		GregorianCalendar gc1 = new GregorianCalendar();
		GregorianCalendar gc2 = new GregorianCalendar();
		gc1.set(YEAR, 2010);
		gc2.set(YEAR, 2010);
		gc1.set(MONTH, 3);
		gc2.set(MONTH, 3);
		gc1.set(DAY_OF_MONTH, 15);
		gc2.set(DAY_OF_MONTH, 15);
		gc1.set(SECOND, 0);
		gc2.set(SECOND, 0);
		gc1.set(MILLISECOND, 0);
		gc2.set(MILLISECOND, 0);

		gc1.set(HOUR_OF_DAY, 8);
		gc1.set(MINUTE, 45);
		gc2.set(HOUR_OF_DAY, 9);
		gc2.set(MINUTE, 30);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal1.addAppointment(app));

		gc1.set(HOUR_OF_DAY, 11);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 12);
		gc2.set(MINUTE, 0);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal1.addAppointment(app));

		gc1.set(HOUR_OF_DAY, 13);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 15);
		gc2.set(MINUTE, 0);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal1.addAppointment(app));

		gc1.set(HOUR_OF_DAY, 9);
		gc1.set(MINUTE, 15);
		gc2.set(HOUR_OF_DAY, 10);
		gc2.set(MINUTE, 0);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal2.addAppointment(app));

		gc1.set(HOUR_OF_DAY, 10);
		gc1.set(MINUTE, 50);
		gc2.set(HOUR_OF_DAY, 12);
		gc2.set(MINUTE, 0);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal2.addAppointment(app));

		gc1.set(HOUR_OF_DAY, 12);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 13);
		gc2.set(MINUTE, 0);
		app = new Appointment();
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		appIds.add(cal3.addAppointment(app));
	}

	protected void tearDown() throws Exception
	{
		Appointment app = new Appointment();
		Connection conn = ConnectionManager.getInstance().getConnection();
		for (int i : appIds)
		{
			app.setId(i);
			try
			{
				app.dbDelete(conn);
			}
			catch (Exception e)
			{
				Assert.fail();
			}
		}

		user1.dbDelete();
		user2.dbDelete();
		user3.dbDelete();
	}

	public void testGetTimeSlots() throws Exception
	{
		GregorianCalendar gc1 = new GregorianCalendar();
		GregorianCalendar gc2 = new GregorianCalendar();
		gc1.set(YEAR, 2010);
		gc2.set(YEAR, 2010);
		gc1.set(MONTH, 3);
		gc2.set(MONTH, 3);
		gc1.set(DAY_OF_MONTH, 15);
		gc2.set(DAY_OF_MONTH, 15);
		gc1.set(SECOND, 0);
		gc2.set(SECOND, 0);
		gc1.set(MILLISECOND, 0);
		gc2.set(MILLISECOND, 0);

		gc1.set(HOUR_OF_DAY, 8);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 16);
		gc2.set(MINUTE, 0);

		List<Period> ts = group.getTimeSlots(12, gc1.getTime(), gc2.getTime(), Granularity.FIFTEEN_MINUTES);

		System.out.println("Time slots:");
		for (Period t : ts)
		{
			System.out.println(t.getStartDate().toString() + " ---> " + t.getEndDate().toString());
		}
	}

	public void testGetTimeSlotsForEmptyGroup() throws Exception
	{
		GroupImpl emptyGroup = new GroupImpl(new ArrayList<String>());

		try
		{
			emptyGroup.getTimeSlots(0, null, null, Granularity.FIFTEEN_MINUTES).size();
			Assert.fail();
		}
		catch (EmptyGroupException e)
		{}
	}

	public void testGrouOperations() throws Exception
	{
		GregorianCalendar gc1 = new GregorianCalendar();
		GregorianCalendar gc2 = new GregorianCalendar();
		gc1.set(YEAR, 2010);
		gc2.set(YEAR, 2010);
		gc1.set(MONTH, 3);
		gc2.set(MONTH, 3);
		gc1.set(DAY_OF_MONTH, 15);
		gc2.set(DAY_OF_MONTH, 15);
		gc1.set(SECOND, 0);
		gc2.set(SECOND, 0);
		gc1.set(MILLISECOND, 0);
		gc2.set(MILLISECOND, 0);

		gc1.set(HOUR_OF_DAY, 10);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 10);
		gc2.set(MINUTE, 45);

		Appointment app = new Appointment();

		app.setDescription("bla bla");
		app.setLocation("moon");
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));

		try
		{
			int appId = group.createGroupAppointment(app);
			group.moveGroupAppointment(appId, new Period(gc1.getTime(), gc2.getTime()));
			group.deleteGroupAppoiment(appId);
		}
		catch (Exception e)
		{
			Assert.fail();
		}

		try
		{
			group.moveGroupAppointment(-1, new Period(gc1.getTime(), gc2.getTime()));
			Assert.fail();
		}
		catch (InvalidAppointmentIdException e)
		{}

		gc1.set(HOUR_OF_DAY, 14);
		gc1.set(MINUTE, 59);
		gc2.set(HOUR_OF_DAY, 15);
		gc2.set(MINUTE, 0);

		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));
		try
		{
			group.createGroupAppointment(app);
			Assert.fail();
		}
		catch (TimeSlotNotAvailableException e)
		{}

		try
		{
			group.moveGroupAppointment(-1, new Period(gc1.getTime(), gc2.getTime()));
			Assert.fail();
		}
		catch (TimeSlotNotAvailableException e)
		{}

		try
		{
			group.deleteGroupAppoiment(-1);
			Assert.fail();
		}
		catch (InvalidAppointmentIdException e)
		{}
	}

	public void testGetGroup() throws Exception
	{
		GregorianCalendar gc1 = new GregorianCalendar();
		GregorianCalendar gc2 = new GregorianCalendar();
		gc1.set(YEAR, 2010);
		gc2.set(YEAR, 2010);
		gc1.set(MONTH, 3);
		gc2.set(MONTH, 3);
		gc1.set(DAY_OF_MONTH, 15);
		gc2.set(DAY_OF_MONTH, 15);
		gc1.set(SECOND, 0);
		gc2.set(SECOND, 0);
		gc1.set(MILLISECOND, 0);
		gc2.set(MILLISECOND, 0);

		gc1.set(HOUR_OF_DAY, 10);
		gc1.set(MINUTE, 0);
		gc2.set(HOUR_OF_DAY, 10);
		gc2.set(MINUTE, 45);

		Appointment app = new Appointment();

		app.setDescription("bla bla");
		app.setLocation("moon");
		app.setTimeSlot(new Period(gc1.getTime(), gc2.getTime()));

		try
		{
			int appId = group.createGroupAppointment(app);
			Group g = GroupImpl.getGroupForAppointment(appId);
			System.out.println("Users in the group:");
			for (String user : g.getGroupUsers())
			{
				System.out.println(user);
			}
			g.deleteGroupAppoiment(appId);
		}
		catch (Exception e)
		{
			Assert.fail();
		}
	}

	public static Test suite()
	{
		return new TestSuite(GroupImplTest.class);
	}

	private GroupImpl group;
	private UserImpl user1;
	private UserImpl user2;
	private UserImpl user3;
	private ArrayList<Integer> appIds;
}