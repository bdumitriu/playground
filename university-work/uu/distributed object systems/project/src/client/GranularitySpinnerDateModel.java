package client;

import javax.swing.*;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

/**
 * @author Bogdan Dumitriu
 * @version 0.1
 * @date Apr 7, 2005
 */
public class GranularitySpinnerDateModel extends SpinnerDateModel
{
	/**
	 * Creates a new GranularitySpinnerDateModel with the specified <code>granularity</code>.
	 *
	 * @param granularity the granularity for this spinner date model.
	 */
	public GranularitySpinnerDateModel(int granularity)
	{
		this(granularity, Calendar.getInstance().getTime());

		specialCase = false;

		Calendar cal = Calendar.getInstance();
		cal.set(Calendar.MILLISECOND, 0);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.HOUR, 0);
		setValue(cal.getTime());
	}

	/**
	 * Creates a new GranularitySpinnerDateModel with the specified <code>granularity</code> &
	 * <code>initialValue</code>.
	 *
	 * @param granularity the granularity for this spinner date model.
	 * @param initialValue the initial value the spinner should display.
	 */
	public GranularitySpinnerDateModel(int granularity, Date initialValue)
	{
		super(initialValue, null, null, Calendar.MINUTE);
		this.granularity = granularity;

		Calendar cal = Calendar.getInstance();
		cal.setTime(initialValue);
		minute = cal.get(Calendar.MINUTE);
		if (minute % granularity != 0)
		{
			specialCase = true;
		}
		else
		{
			specialCase = false;
		}
	}

	public Object getNextValue()
	{
		Calendar cal = Calendar.getInstance();
		cal.setTime((Date) getValue());
		if (specialCase)
		{
			cal.add(Calendar.MINUTE, granularity - (minute % granularity));
			specialCase = false;
		}
		else
		{
			cal.add(getCalendarField(), getDelta());
		}
		Date next = cal.getTime();
		return ((getEnd() == null) || (getEnd().compareTo(next) >= 0)) ? next : null;
	}

	public Object getPreviousValue()
	{
		Calendar cal = Calendar.getInstance();
		cal.setTime((Date) getValue());
		if (specialCase)
		{
			cal.add(Calendar.MINUTE, -(minute % granularity));
			specialCase = false;
		}
		else
		{
			cal.add(getCalendarField(), -getDelta());
		}
		Date prev = cal.getTime();
		return ((getStart() == null) || (getStart().compareTo(prev) <= 0)) ? prev : null;
	}

	private int getDelta()
	{
		int calField = getCalendarField();
		if (calField == Calendar.MINUTE)
		{
			return granularity;
		}
		else
		{
			return 1;
		}
	}

	private int granularity;
	private boolean specialCase;
	private int minute;
}
