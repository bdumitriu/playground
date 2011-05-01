package core.test;

/**
 *
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 10, 2005
 */

import junit.framework.*;
import core.*;
import java.util.List;

public class AppointmentFinderTest extends TestCase
{
	protected void setUp() throws Exception
	{
		appFinder = AppointmentFinder.getInstance();
	}

	public void testFind() throws Exception
	{
		Appointment app = appFinder.find("1");
		Assert.assertEquals(app.getTimeSlot().getStartDate().getTime(), 1110456900000L);
		Assert.assertEquals(app.getTimeSlot().getEndDate().getTime(), 1110463200000L);
		Assert.assertEquals(app.getDescription(), "PT class");
		Assert.assertEquals(app.getLocation(), "BBL-???");
		Assert.assertEquals(app.isGroupAppointment(), false);
	}

	public void testNoFind() throws Exception
	{
		Appointment app = appFinder.find("gjerkgegr lrejgrejgrl");
		Assert.assertEquals(app, null);
	}

	public void testLoadedFind() throws Exception
	{
		Appointment firstApp = appFinder.find("1");
		Appointment secondApp = appFinder.find("1");
		Assert.assertEquals(firstApp, secondApp);
	}

	public void testFindForUser() throws Exception
	{
		List<Appointment> apps = appFinder.findForUser("bdumitriu");
		Assert.assertEquals(apps.size(), 3);

		Appointment app1 = appFinder.find("1");
		Appointment app2 = appFinder.find("2");
		Appointment app3 = appFinder.find("3");

		Assert.assertEquals(app1, apps.get(0));
		Assert.assertEquals(app2, apps.get(1));
		Assert.assertEquals(app3, apps.get(2));
	}

	public void testReverseFindForUser() throws Exception
	{
		Appointment app1 = appFinder.find("1");
		Appointment app2 = appFinder.find("2");
		Appointment app3 = appFinder.find("3");

		List<Appointment> apps = appFinder.findForUser("bdumitriu");

		Assert.assertEquals(app1, apps.get(0));
		Assert.assertEquals(app2, apps.get(1));
		Assert.assertEquals(app3, apps.get(2));
	}

	public static Test suite()
	{
		return new TestSuite(AppointmentFinderTest.class);
	}

	AppointmentFinder appFinder;
}