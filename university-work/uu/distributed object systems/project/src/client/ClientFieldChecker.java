package client;

public class ClientFieldChecker
{
	
	/**
	 * 
	 * This class will check Form fields client side.
	 *
	 * @author Richard Nieuwenhuis
	 * @version 0.1
	 * @date Apr 11, 2005
	 */

	public ClientFieldChecker()
	{

	}
	
	/**
	 * Checks the field of the login form
	 * 
	 * @param loginname
	 * @param password	
	 */

	public boolean checkLoginFields(String loginName, char[] password)
	{
		if (loginName.equals("") || password.length == 0)
		{
			return false;
		}

		else
		{
			return true;
		}
	}
	
	/**
	 * Checks the field of the form for creating a new account
	 * 
	 * @param name
	 * @param loginname
	 * @param title
	 * @param password
	 * @param phone number
	 * @param password 1
	 * @param password 2	
	 */

	public boolean checkAccountFields(String name, String login, String title, String number, char[] p1, char[] p2)
	{
		boolean b = true;
		if (name.equals("") || login.equals("") || title.equals("") || number.equals("") || p1.length == 0 || p2.length == 0)
		{
			b = false;
		}

		String pass1 = new String(p1);
		String pass2 = new String(p2);

		if (!pass1.equals(pass2))
		{
			b = false;
		}

		for (int i = 0; i < number.length(); i++)
		{
			if (number.charAt(i) < '0' || number.charAt(i) > '9')
			{
				b = false;
			}
		}
		return b;
	}

	public String checkSingleAppointmentFields(int day)
	{
		String message = "";
		if (day == -1)
		{
			message += "No day selected";
		}

		return message;
	}

	/**
	 * Checks the field of the create group appointment form
	 * 
	 * @param number of selected users
	 * @param the duration in minutes
	 * @param description
	 * @param location	
	 */
	public String checkGroupAppointment(int size, String mins, String des, String loc)
	{
		String message = "";
		if (size == 0)
		{
			message += "No users selected\n";
		}

		try
		{
			Integer.parseInt(mins);
		}
		catch (Exception es)
		{
			message += "Not valid duration\n";
		}

		if (des.equals("") || loc.equals(""))
		{
			message += "Some fields are empty\n";
		}

		return message;
	}

	public String getPassword(char[] password)
	{
		return new String(password);
	}
}