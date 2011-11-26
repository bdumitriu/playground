package RMI.hello;

import java.rmi.*;
import java.rmi.server.*;
import java.rmi.registry.*;
import java.util.*;

public class HelloServer extends UnicastRemoteObject implements Hello
{
	public static void main(String args[])
	{
		//System.setSecurityManager(new RMISecurityManager());
		try
		{
			HelloServer serverObject = new HelloServer();
			Registry reg = LocateRegistry.createRegistry(5037);
			Naming.rebind("//192.168.0.2:5037/helloServer", serverObject);
			//Naming.rebind("//192.168.0.2/helloServer", serverObject);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public HelloServer() throws RemoteException
	{
		super();
	}
	
	public String sayHello() throws RemoteException
	{
		String message = "Hello! Next time give your mother tongue ";
		message += "as an argument.";
		
		return message;
	}
	
	public String polyglotGreetings(String lang) throws RemoteException
	{
		String message = "";
		String deLa = "";
		String h = "";
		String userName = "Bogdan";
		//Properties ps = System.getProperties();
		
		//userName = ps.getProperty("user.name").toUpperCase();
		lang = lang.toLowerCase();
		
		try
		{
			h = RemoteServer.getClientHost();
		}
		catch (ServerNotActiveException e)
		{
			e.printStackTrace();
		}
		
		if (lang.startsWith("romana"))
		{
			message = "Buna ziua! Incantat de cunostinta, ";
			deLa = " de la ";
		}
		else if (lang.startsWith("english"))
		{
			message = "Hello! Pleased to meet you, ";
			deLa = " from ";
		}
		else if (lang.startsWith("deutsch"))
		{
			message = "Hallo! Freu dir zu treffen, ";
			deLa = " aus ";
		}
		else
		{
			message = lang + "  Oops! Don't know this language! ";
			message += "I only know: romana, english, deutsch. ";
			deLa = " from ";
		}
		
		return message + userName + deLa + h + "!";
	}
}
