package client;

import java.util.*;
import java.lang.Math;

import core.GroupwareManagement;
import core.UserData;
import core.Granularity;
import core.exceptions.DuplicateLoginNameException;
import core.Appointment;

public class ClientModel extends Observable{

	/**
	 * 
	 * This class is used to contain and set the dates.  	 * 
	 *
	 * @author Richard Nieuwenhuis
	 * @version 0.1
	 * @date Apr 11, 2005
	 */
	
	private Calendar calendar;
	private int currentMonth, currentYear, day;
	private int granularity = 30;
	private Date wantedDate;
	private List<Appointment> monthApps;
	
	public ClientModel()
	{
		calendar     = new GregorianCalendar();
		currentMonth = calendar.get(Calendar.MONTH);
		currentYear  = calendar.get(Calendar.YEAR);
		setToDate(currentMonth, currentYear);
		day          = -1;
	}
	
	//set the calendar to the date the user has selected
	public void setToDate(int month, int year, int day)
	{
		calendar.set(year, month, day);
	}
	
	public void setToDate(int month, int year)
	{
		day = -1;
		calendar.set(year, month, 1);
	}
	
	public void setToDate(Date date)
	{
		calendar.setTime(date);
	}
	
	public void setToDate(Date date, String s)
	{
		System.out.println(s);
		calendar.setTime(date);
	}
	
	//set to the real date we live in
	public void setToRealCurrentDate()
	{
		calendar.set(getCurrentYear(), getCurrentMonth(), 1);
	}
	
	public void setDay(int day)
	{
		System.out.println("Day: " + day);
		this.day = day;
	}
	
	//these two methods are for getting the actual current month and year
	public int getCurrentMonth()
	{
		return new GregorianCalendar().get(GregorianCalendar.MONTH);
	}
	
	public int getCurrentYear()
	{
		return new GregorianCalendar().get(GregorianCalendar.YEAR);
	}
	
	//these two methods are for getting the year and month the calendar has
	//been set to
	public int getMonth()
	{  	
		//System.out.println("We are now in month:" + calendar.get(Calendar.MONTH));
		return calendar.get(Calendar.MONTH);
	}
	
	public int getYear()
	{
		//System.out.println("We are now in year:" + calendar.get(Calendar.YEAR));
		return calendar.get(Calendar.YEAR);
	}
	
	public int getDay()
	{
		System.out.println("Day of Month: " + calendar.get(Calendar.DAY_OF_MONTH));
		return calendar.get(Calendar.DAY_OF_MONTH);
	}
	
	public int getSelDay()
	{
		return day;
	}
	
	public int getHour()
	{
		return calendar.get(Calendar.HOUR_OF_DAY);
	}
	
	public int getMinute()
	{
		return calendar.get(Calendar.MINUTE);
	}
	
	public String getDateRep()
	{
		return getDay() + "-" + (getMonth()+1) + "-" + getYear() + " at " +
			getHour() + ":" + getMinute();
	}
	
	public int getDaysOfMonth()
	{
		return calendar.getActualMaximum(Calendar.DAY_OF_MONTH);
	}
	
	public Date getDate()
	{
		return calendar.getTime();
	}
	
	//when making appointments its usefull to remember a certain date
	//to display the wanted day with appointments
	public void setToWantedDate()
	{
		wantedDate = getDate();
	}
	
	public Date getWantedDate()
	{
		return wantedDate;
	}
	
	public Date getDateForAppointment(int year, int month, int day, int hours, int mins)
	{
		//calendar.set(getYear(), getMonth(), getDay(), hours, mins, 0);
		calendar.set(Calendar.YEAR, year);
		calendar.set(Calendar.MONTH, month);
		calendar.set(Calendar.DAY_OF_MONTH, day);
		calendar.set(Calendar.HOUR_OF_DAY, hours);
		calendar.set(Calendar.MINUTE, mins);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);
		return calendar.getTime();	
	}

	public int getFirstDayOfWeek()
	{
		calendar.set(Calendar.DAY_OF_MONTH, 1);
		int day = calendar.get(Calendar.DAY_OF_WEEK) - 1;
		if (day == 0)
		{
			day = 7;
		}
		return day;
	}

 /** to remember which granularity he had selected
 *  it possible he selected other granularuty while making his appointment
 *  
 *  @param granularity
 */
	public void setGranularity(int gran)
	{
		granularity = gran;
	}
	
	public int getGranularityInTime()
	{
		if(granularity==1)
			return 60;
		else if (granularity==2)
			return 30;
		else if (granularity==4)
			return 15;
		else
			return 30;
	}
	
	public int getGranularity()
	{
		return granularity;
	}	
}

