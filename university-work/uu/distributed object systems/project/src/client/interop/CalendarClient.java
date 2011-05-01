package client.interop;

import static java.util.Calendar.*;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.CosNaming.NamingContextExt;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 10, 2005
 */
public class CalendarClient
{
	public static void main(String[] args)
	{
		try
		{
			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			// get the root naming context
			org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
			NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);

			// resolve the object reference in Naming
			String name = "calendar";
			calendar = CalendarHelper.narrow(ncRef.resolve_str(name));

			System.out.println("To get your appointments, please supply your login name and password.\n");

			BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
			System.out.print("Login name: ");
			String loginName = br.readLine();
			System.out.print("Password: ");
			String password = br.readLine();

			Appointment[] apps = calendar.getAppointments(loginName, password);
			System.out.println("\nThe appointments in your calendar are:\n");
			for (int i = 0; i < apps.length; i++)
			{
				printAppointment(apps[i]);
			}
		}
		catch (Exception e)
		{
			System.out.println("Error: " + e);
			e.printStackTrace(System.out);
		}
	}

	private static void printAppointment(Appointment app)
	{
		System.out.println("Location    : " + app.location);
		System.out.println("Description : " + app.description);
		System.out.println("Form        : " + getDate(app.startDate));
		System.out.println("To          : " + getDate(app.endDate));
		if (app.isGroupAppointment)
		{
			System.out.println("Type        : group appointment");
		}
		else
		{
			System.out.println("Type        : single appointment");
		}
		System.out.println();
	}

	private static String getDate(Date date)
	{
		java.util.Calendar cal = java.util.Calendar.getInstance();
		cal.set(SECOND, date.second);
		cal.set(MINUTE, date.minute);
		cal.set(HOUR, date.hour);
		cal.set(DAY_OF_MONTH, date.day);
		cal.set(MONTH, date.month - 1);
		cal.set(YEAR, date.year);

		return cal.getTime().toString();
	}

	static Calendar calendar;
}
