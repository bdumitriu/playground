package RMI.hello;

import java.rmi.*;
import java.rmi.registry.*;

public class HelloClient
{
	static String message = "";

	public static void main(String args[])
	{
		try
		{
			//Registry reg = LocateRegistry.getRegistry(
			//	"192.168.0.2", 5037);
			//String cont[] = reg.list();
			//Hello clientObject = (Hello) reg.lookup("//192.168.0.2/helloServer");
			Hello clientObject = (Hello) Naming.lookup("//192.168.0.2:5037/helloServer");
			String cont[] = Naming.list("rmi://192.168.0.2:5037");
			//String cont[] = Naming.list("rmi://192.168.0.2");
			//Hello clientObject = (Hello) Naming.lookup("//192.168.0.2/helloServer");
			
			for (int i = 0; i < cont.length; i++)
			{
				System.out.println(cont[i]);
			}
			
			if (args.length > 0)
				message = clientObject.polyglotGreetings(
					args[0]);
			else
				message = clientObject.sayHello();

			System.out.println();
			System.out.println(message);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
}
