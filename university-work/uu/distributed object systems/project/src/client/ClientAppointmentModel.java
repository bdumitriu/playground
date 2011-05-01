package client;
import core.Appointment;
import core.Period;

import java.util.*;
import java.io.Serializable;

public class ClientAppointmentModel implements Observer, Serializable {

	private List<Appointment> apps;
	private Observer obs;
	
	public ClientAppointmentModel(Observer obs)
	{
		this.obs = obs;
		try
		{
			client = GroupwareClient.getInstance();
		}
		catch (GroupwareClientException e)
		{}
	}

	public void setAppointments(List<Appointment> apps)
	{
		this.apps = apps;
	}

	public boolean hasAppointment(int year, int month, int day)
	{
		for (Appointment app : apps)
		{
			Period period = app.getTimeSlot();
			Date date = period.getStartDate();
			java.util.Calendar cal = java.util.Calendar.getInstance();
			cal.setTime(date);
			if (cal.get(java.util.Calendar.YEAR) != year)
			{
				continue;
			}
			if (cal.get(java.util.Calendar.MONTH) != month)
			{
				continue;
			}
			if (cal.get(java.util.Calendar.DAY_OF_MONTH) != day)
			{
				continue;
			}
			return true;
		}
		return false;
	}

	public List<Appointment> getAppointments(int year, int month, int day)
	{
		List<Appointment> monthApps = new ArrayList<Appointment>();

		for (Appointment app : apps)
		{
			Period period = app.getTimeSlot();
			java.util.Calendar cal = java.util.Calendar.getInstance();
			cal.setTime(period.getStartDate());
			if (cal.get(java.util.Calendar.YEAR) != year)
			{
				continue;
			}
			if (cal.get(java.util.Calendar.MONTH) != month)
			{
				continue;
			}
			if (cal.get(java.util.Calendar.DAY_OF_MONTH) != day)
			{
				continue;
			}
			monthApps.add(app);
		}
		return monthApps;
	}
	
	public Appointment getNewAppointment(int appointmentId)
	{
		for (Appointment app : apps)
		{
			if (app.getId() == appointmentId)
			{
				return app;
			}
		}

		return null;
	}

	public void increaseUpdateCount()
	{
		updateCount++;
	}

	public void decreaseUpdateCount()
	{
		if (updateCount > 0)
		{
			updateCount--;
		}
		else
		{
			updateCount = 0;
		}
	}

	public void update(Observable observable, Object o)
	{
		System.out.println("Received callback from server.");

		if (updateCount == 0)
		{
			System.out.print("Updating...");
			System.out.println((Integer) o);
			try
			{
				apps = client.getAllAppointments();
				obs.update(null, o);
			}
			catch (GroupwareClientException e)
			{}
		}
		else
		{
			decreaseUpdateCount();
		}
	}

	private GroupwareClient client;
	private int updateCount = 0;
}
