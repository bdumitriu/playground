package core.interop;

import static java.util.Calendar.*;

import core.GroupwareManagementImpl;
import core.User;
import core.UserFinder;

import java.util.List;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 10, 2005
 */
public class CalendarImpl extends CalendarPOA
{
	public CalendarImpl()
	{
		super();
	}

	public Appointment[] getAppointments(String loginName, String password)
	{
		Appointment[] result = new Appointment[0];
		try
		{
			User u = UserFinder.getInstance().findIfPassword(loginName, password);
			if (u != null)
			{
				List<core.Appointment> apps = u.getCalendar().getAllAppointments();

				result = new Appointment[apps.size()];
				int i = 0;
				for (core.Appointment app : apps)
				{
					Appointment simpleApp = new Appointment(
						app.getId(),
						createDate(app.getTimeSlot().getStartDate()),
						createDate(app.getTimeSlot().getEndDate()),
						app.getLocation(),
						app.getDescription(),
						app.isGroupAppointment());
					result[i++] = simpleApp;
				}

				//gm.logOff(u.getAuthToken());
			}
		}
		catch (Exception e)
		{}
		finally
		{
			return result;
		}
	}

	private Date createDate(java.util.Date date)
	{
		java.util.Calendar cal = java.util.Calendar.getInstance();
		cal.setTime(date);

		return new Date(cal.get(SECOND), cal.get(MINUTE), cal.get(HOUR),
			cal.get(DAY_OF_MONTH), cal.get(MONTH) + 1, cal.get(YEAR));
	}
}
