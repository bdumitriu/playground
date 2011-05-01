package ro.utcluj.vu.chat;

import javax.swing.JApplet;

/**
 * This is the applet that embedds the client. It will be sent via a WEB page
 * and deployed on the client side.
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */
public class ClientApplet extends JApplet
{
	Client client;

	public void init()
	{
		super.init();
		try
		{
			// get parameters from the web page
			String userID = getParameter("CHAT_USERID");
			System.out.println(userID);
			String classID = getParameter("CHAT_CLASSID");
			System.out.println(classID);
			String userName = getParameter("CHAT_USERNAME");
			System.out.println(userName);
			String className = getParameter("CHAT_CLASSNAME");
			System.out.println(className);

			//create the client
			client = new Client(userID, classID, userName, className);
			client.setSelf();

			String address = getParameter("CHAT_ADDRESS");
			int port = Integer.parseInt(getParameter("CHAT_PORT"));

			client.setAdressAttributes(address, port);
			client.rmiConnect();

		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void start()
	{
		super.start();
	}

	public void stop()
	{
		super.stop();
	}

	public void destroy()
	{
		super.destroy();
	}
}
