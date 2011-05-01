import java.util.Date;
import java.util.Calendar;
import java.util.GregorianCalendar;

public class Announce
{
	public static void main(String args[])
	{
		MyDate birthdays[] = {new MyDate("Dana", (byte) 24, (byte) 6),
				      new MyDate("Bogdan&Dana", (byte) 5, (byte) 4),
				      new MyDate("Bogdan&Dana/2", (byte) 5, (byte) 10),
				      new MyDate("mama", (byte) 5, (byte) 8),
				      new MyDate("tata", (byte) 24, (byte) 7),
				      new MyDate("Titus", (byte) 19, (byte) 11),
				      new MyDate("Deni", (byte) 28, (byte) 6),
				      new MyDate("Anda", (byte) 6, (byte) 3),
				      new MyDate("Ana", (byte) 27, (byte) 9),
				      new MyDate("Draga", (byte) 14, (byte) 4),
				      new MyDate("Bidi", (byte) 30, (byte) 1),
				      new MyDate("Zoe", (byte) 1, (byte) 7),
				      new MyDate("Titi", (byte) 22, (byte) 1),
				      new MyDate("Danut", (byte) 2, (byte) 7),
				      new MyDate("Andrei", (byte) 16, (byte) 5),
				      new MyDate("Stanca", (byte) 12, (byte) 5),
				      new MyDate("Petra", (byte) 14, (byte) 8),
				      new MyDate("Sandu", (byte) 16, (byte) 11),
				      new MyDate("Silvia", (byte) 26, (byte) 12),
				      new MyDate("Bianca", (byte) 26, (byte) 7),
				      new MyDate("Tudor", (byte) 9, (byte) 12),
				      new MyDate("Andi", (byte) 16, (byte) 4),
				      new MyDate("Ilinca", (byte) 11, (byte) 11),
				      new MyDate("Hori", (byte) 28, (byte) 7),
				      new MyDate("Adi", (byte) 27, (byte) 8),
				      new MyDate("Mircea", (byte) 31, (byte) 12),
				      new MyDate("Pinky", (byte) 17, (byte) 5),
				      new MyDate("Mita", (byte) 13, (byte) 11),
				      new MyDate("Grasi", (byte) 29, (byte) 11),
				      new MyDate("X_", (byte) 1, (byte) 11),
				      new MyDate("Kit", (byte) 3, (byte) 1),
				      new MyDate("Mihai", (byte) 5, (byte) 5),
				      new MyDate("tatal Danei", (byte) 27, (byte) 4),
				      new MyDate("mama Danei", (byte) 10, (byte) 4)
				};

		GregorianCalendar cal = new GregorianCalendar();
		cal.setTime(new Date(System.currentTimeMillis()));
		cal.add(Calendar.DAY_OF_MONTH, 1);
		MyDate tomorrow = new MyDate(cal.getTime());
		MyDate today = new MyDate(new Date(System.currentTimeMillis()));
		boolean test = false;

		for (int i = 0; i < birthdays.length; i++)
		{
			if (birthdays[i].equals(today))
			{
				System.out.println("AI GRIJA, AZI E ZIUA LUI " + 
					birthdays[i].getName() + "!");
				test = true;
			}
			if (birthdays[i].equals(tomorrow))
			{
				System.out.println("AI GRIJA, MAINE E ZIUA LUI " + 
					birthdays[i].getName() + "!");
				test = true;
			}
		}

		if (test)
			try
			{
				System.in.skip(System.in.available());
				System.in.read();
			}
			catch (Exception e)
			{}
	}
}

class MyDate
{
	private String name;
	private byte day;
	private byte month;

	public MyDate(Date date)
	{
		GregorianCalendar cal = new GregorianCalendar();
		cal.setTime(date);
		day = (byte) cal.get(Calendar.DAY_OF_MONTH);
		month = (byte) (cal.get(Calendar.MONTH)+1);
		name = "";
	}

	public MyDate(String name, byte day, byte month)
	{
		this.name = name;
		this.day = day;
		this.month = month;
	}

	public String getName()
	{
		return name;
	}

	public byte getDay()
	{
		return day;
	}

	public byte getMonth()
	{
		return month;
	}

	public boolean equals(Object obj)
	{
		if (!(obj instanceof MyDate))
			return false;

		MyDate date = (MyDate) obj;

		if ((day == date.day) && (month == date.month))
			return true;
		else
			return false;
	}

	public String toString()
	{
		return name + ", " + new Byte(day).toString() + "." + new Byte(month).toString();
	}
}
