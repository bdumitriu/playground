package core;

import java.util.Date;
import java.io.Serializable;

/**
 * A basic class which defines a period by its start and end dates.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Mar 10, 2005
 */
public class Period implements Serializable
{
	public Period()
	{
	}

	public Period(Date startDate, Date endDate)
	{
		this.startDate = startDate;
		this.endDate = endDate;
	}

	public Date getStartDate()
	{
		return startDate;
	}

	public void setStartDate(Date startDate)
	{
		this.startDate = startDate;
	}

	public Date getEndDate()
	{
		return endDate;
	}

	public void setEndDate(Date endDate)
	{
		this.endDate = endDate;
	}

	private Date startDate;
	private Date endDate;
}
