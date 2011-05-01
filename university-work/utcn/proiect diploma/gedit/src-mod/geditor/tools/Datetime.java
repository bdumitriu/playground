package geditor.tools;

import org.w3c.dom.*;

import java.util.GregorianCalendar;
import java.io.Serializable;

/**
 * This is a container class for representing the date and time.
 * <br /><br />
 * Date: Feb 26, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class Datetime implements Serializable
{
	/**
	 * Creates a new Datetime object and initializes it with the current system time.
	 */
	public Datetime()
	{
		GregorianCalendar gcal = new GregorianCalendar();

		year = (short) gcal.get(GregorianCalendar.YEAR);
		month = (byte) (gcal.get(GregorianCalendar.MONTH) + 1);
		day = (byte) gcal.get(GregorianCalendar.DAY_OF_MONTH);
		hour = (byte) gcal.get(GregorianCalendar.HOUR_OF_DAY);
		minute = (byte) gcal.get(GregorianCalendar.MINUTE);
		second = (byte) gcal.get(GregorianCalendar.SECOND);
		milli = (short) gcal.get(GregorianCalendar.MILLISECOND);
	}

	/**
	 * Creates a representation of this Datetime as an Element node of a DOM Document and returns it.
	 *
	 * @param doc the Document to use in order to generate instances of Node's
	 * @return the generated Element.
	 */
	public Node toDocumentElement(Document doc)
	{
		Element root = doc.createElement("timestamp");

		Element temp;

		temp = doc.createElement("year");
		temp.appendChild(doc.createTextNode((new Short(year)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("month");
		temp.appendChild(doc.createTextNode((new Byte(month)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("day");
		temp.appendChild(doc.createTextNode((new Byte(day)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("hour");
		temp.appendChild(doc.createTextNode((new Byte(hour)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("minute");
		temp.appendChild(doc.createTextNode((new Byte(minute)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("second");
		temp.appendChild(doc.createTextNode((new Byte(second)).toString()));
		root.appendChild(temp);

		temp = doc.createElement("milli");
		temp.appendChild(doc.createTextNode((new Short(milli)).toString()));
		root.appendChild(temp);

        	return root;
	}

	/**
	 * Modifies the members of this Datetime with the values read from an element Node of a DOM Document. The
	 * element node has to be the one containing the "timestamp" tag.
	 * <br /><br />
	 * @param root the root node of the subtree containig data about this timestamp
	 * @return true if all went ok, false if data members could not be properly initialized. Beware that some of
	 *	the values might have changed while some might not if false is returned.
	 */
	public boolean fromDocumentElement(Node root)
	{
		NodeList timestampData = root.getChildNodes();

		// first try to find out the indices of each of the elements we are interested in
		int dataPosition[] = new int[7];
		for (int i = 0; i < 7; i++)
		{
			dataPosition[i] = -1;
		}
		for (int i = 0; i < timestampData.getLength(); i++)
		{
			Node currentData = timestampData.item(i);
			if (currentData.getNodeType() == Node.ELEMENT_NODE)
			{
				String nodeValue = currentData.getNodeName();
				if (nodeValue.equals("year"))
				{
					dataPosition[0] = i;
				}
				else if (nodeValue.equals("month"))
				{
					dataPosition[1] = i;
				}
				else if (nodeValue.equals("day"))
				{
					dataPosition[2] = i;
				}
				else if (nodeValue.equals("hour"))
				{
					dataPosition[3] = i;
				}
				else if (nodeValue.equals("minute"))
				{
					dataPosition[4] = i;
				}
				else if (nodeValue.equals("second"))
				{
					dataPosition[5] = i;
				}
				else if (nodeValue.equals("milli"))
				{
					dataPosition[6] = i;
				}
			}
		}

		// first check that all the elements exist
		for (int i = 0; i < 7; i++)
		{
			if (dataPosition[i] == -1)
			{
				return false;
			}
		}

		// now that we know the positions, we can extract the values
		try
		{
			String tmp;

			// extract the year
			tmp = timestampData.item(dataPosition[0]).getFirstChild().getNodeValue();
			year = new Short(tmp).shortValue();

			// extract the month
			tmp = timestampData.item(dataPosition[1]).getFirstChild().getNodeValue();
			month = new Byte(tmp).byteValue();

			// extract the day
			tmp = timestampData.item(dataPosition[2]).getFirstChild().getNodeValue();
			day = new Byte(tmp).byteValue();

			// extract the hour
			tmp = timestampData.item(dataPosition[3]).getFirstChild().getNodeValue();
			hour = new Byte(tmp).byteValue();

			// extract the minute
			tmp = timestampData.item(dataPosition[4]).getFirstChild().getNodeValue();
			minute = new Byte(tmp).byteValue();

			// extract the second
			tmp = timestampData.item(dataPosition[5]).getFirstChild().getNodeValue();
			second = new Byte(tmp).byteValue();

			// extract the milli
			tmp = timestampData.item(dataPosition[6]).getFirstChild().getNodeValue();
			milli = new Short(tmp).shortValue();
		}
		catch (NumberFormatException e)
		{
			e.printStackTrace();
			return false;
		}
		catch (DOMException e)
		{
			e.printStackTrace();
			return false;
		}

		return true;
	}

	/**
	 * Returns true if this Datetime is lower than the specified <code>timestamp</code> and false otherwise.
	 * @param timestamp the timestamp to compare this Datetime with
	 * @return true if this Datetime is lower than the specified <code>timestamp</code> and false otherwise
	 */
	public boolean lowerThan(Datetime timestamp)
	{
		return (Datetime.compare(this, timestamp) == -1);
	}

	/**
	 * Returns true if this Datetime is higher than the specified <code>timestamp</code> and false otherwise.
	 * @param timestamp the timestamp to compare this Datetime with
	 * @return true if this Datetime is higher than the specified <code>timestamp</code> and false otherwise
	 */
	public boolean higherThan(Datetime timestamp)
	{
		return (Datetime.compare(this, timestamp) == 1);
	}

	public short getYear()
	{
		return year;
	}

	public void setYear(short year)
	{
		this.year = year;
	}

	public byte getMonth()
	{
		return month;
	}

	public void setMonth(byte month)
	{
		this.month = month;
	}

	public byte getDay()
	{
		return day;
	}

	public void setDay(byte day)
	{
		this.day = day;
	}

	public byte getHour()
	{
		return hour;
	}

	public void setHour(byte hour)
	{
		this.hour = hour;
	}

	public byte getMinute()
	{
		return minute;
	}

	public void setMinute(byte minute)
	{
		this.minute = minute;
	}

	public byte getSecond()
	{
		return second;
	}

	public void setSecond(byte second)
	{
		this.second = second;
	}

	public short getMilli()
	{
		return milli;
	}

	public void setMilli(short milli)
	{
		this.milli = milli;
	}

	public String toString()
	{
		StringBuffer sb = new StringBuffer();

		sb.append("date: ");
		sb.append(day);
		sb.append(".");
		sb.append(month);
		sb.append(".");
		sb.append(year);
		sb.append(" time: ");
		sb.append(hour);
		sb.append(":");
		sb.append(minute);
		sb.append(":");
		sb.append(second);
		sb.append(".");
		sb.append(milli);

		return sb.toString();
	}

	public boolean equals(Object obj)
	{
		if (!(obj instanceof Datetime))
		{
			return false;
		}

		return (Datetime.compare(this, (Datetime) obj) == 0);
	}

	/**
	 * Compares the two Datetime's and returns -1 if dt1 < dt2, 0 if they are equal and 1 if dt1 > dt2.
	 */
	private static short compare(Datetime dt1, Datetime dt2)
	{
		if (dt1.year < dt2.year)
		{
			return -1;
		}
		else if (dt1.year > dt2.year)
		{
			return 1;
		}

		if (dt1.month < dt2.month)
		{
			return -1;
		}
		else if (dt1.month > dt2.month)
		{
			return 1;
		}

		if (dt1.day < dt2.day)
		{
			return -1;
		}
		else if (dt1.day > dt2.day)
		{
			return 1;
		}

		if (dt1.hour < dt2.hour)
		{
			return -1;
		}
		else if (dt1.hour > dt2.hour)
		{
			return 1;
		}

		if (dt1.minute < dt2.minute)
		{
			return -1;
		}
		else if (dt1.minute > dt2.minute)
		{
			return 1;
		}

		if (dt1.second < dt2.second)
		{
			return -1;
		}
		else if (dt1.second > dt2.second)
		{
			return 1;
		}

		if (dt1.milli < dt2.milli)
		{
			return -1;
		}
		else if (dt1.milli > dt2.milli)
		{
			return 1;
		}

		return 0;
	}

	private short year;
	private byte month;
	private byte day;
	private byte hour;
	private byte minute;
	private byte second;
	private short milli;
}
