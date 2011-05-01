import java.net.*;

public class InternetAddress
{
	public static void main(String args[])
	{
		try
		{
			InetAddress ia = InetAddress.getByName("blackwizard");
			
			System.out.println(ia.getHostAddress());
			System.out.println(ia.getHostName());
		}
		catch (UnknownHostException e)
		{
			System.out.println(e.toString());
		}
	}
}
