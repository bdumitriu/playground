import java.util.Vector;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class MailListServlet extends HttpServlet
{
	private Vector addresses;
	private String filename;

	public void init(ServletConfig config) throws ServletException
	{
		super.init(config);
		filename = config.getInitParameter("addressfile");
		if (filename == null)
			throw new UnavailableException(this, "The \"addressfile\" property "+
				"must be set to a filename!" + config.getInitParameter("addressfile"));
		try
		{
			ObjectInputStream in = new ObjectInputStream(new FileInputStream(filename));
			addresses = (Vector) in.readObject();
			in.close();
		}
		catch (FileNotFoundException e)
		{
			addresses = new Vector();
		}
		catch (Exception e)
		{
			throw new UnavailableException("Error reading address file!" + e.toString());
		}
	}

	protected void doGet(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		res.setContentType("text/html");
		res.setHeader("pragma", "no-cache");
		
		PrintWriter out = res.getWriter();
		
		out.print("<HTML><HEAD><TITLE>List Manager</TITLE></HEAD>");
 		out.print("<BODY><H3>Members:</H3><UL>");
 		for (int i = 0; i < addresses.size(); i++)
 			out.print("<LI>" + addresses.elementAt(i));
 		out.print("</UL><HR><FORM METHOD=POST>");
 		out.print("Enter your e-mail address: <INPUT TYPE=TEXT NAME=email><BR>");
 		out.print("<INPUT TYPE=SUBMIT NAME=action VALUE=subscribe>");
 		out.print("<INPUT TYPE=SUBMIT NAME=action VALUE=unsubscribe>");
 		out.print("</FORM></BODY></HTML>");
 		
 		out.close();
	}

	protected void doPost(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		String email = req.getParameter("email");
		String msg;
		
		if (email.equals(""))
		{
			sendError(res, "No email address specified!");
			return;
		}
		if (req.getParameter("action").equals("subscribe"))
			if (subscribe(email))
				msg = "Address " + email + " has been subscribed.";
			else
			{
				sendError(res, "Address " + email + " was already subscribed!");
				return;
			}
		else
			if (unsubscribe(email))
				msg = "Address " + email + " has been unsubscribed.";
			else
			{
				sendError(res, "There was nothing to unsubscribe!");
				return;
			}
		
		res.setContentType("text/html");
		res.setHeader("pragma", "no-cache");
		PrintWriter out = res.getWriter();
		out.print("<HTML><HEAD><TITLE>List Manager</TITLE></HEAD><BODY>");
		out.print(msg);
		out.print("<HR><A HREF=\"");
		out.print(req.getRequestURI());
		out.print("\">Show the list</A></BODY></HTML>");
		out.close();
	}

	private synchronized boolean subscribe(String email) throws IOException
	{
		if (addresses.contains(email))
			return false;
			
		addresses.addElement(email);
		save();
		return true;
	}

	private synchronized boolean unsubscribe(String email) throws IOException
	{
		if (!addresses.removeElement(email))
			return false;
			
		save();
		return true;
	}

	private void save() throws IOException
	{
		ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filename));
		out.writeObject(addresses);
		out.close();
	}

	private void sendError(HttpServletResponse res, String message) throws IOException
	{
		res.setContentType("text/html");
		res.setHeader("pragma", "no-cache");
		PrintWriter out = res.getWriter();
		out.println("<HTML><HEAD><TITLE>Error!</TITLE></HEAD>");
		out.println("<BODY><BR>" + message + "</BODY></HTML>");
		return;
	}
}

/*
import java.util.Vector;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
 
public class MailListServlet extends HttpServlet
{
	private Vector addresses;
	private String filename;

	public void init(ServletConfig config) throws ServletException
	{
		super.init(config);
		filename = config.getInitParameter("addressfile");
		if(filename == null)
			throw new UnavailableException(this,
					"The \"addressfile\" property "+
					"must be set to a file name");
		try
		{
			ObjectInputStream in =
				new ObjectInputStream(new FileInputStream(filename));
			addresses = (Vector)in.readObject();
			in.close();
		}
		catch(FileNotFoundException e) { addresses = new Vector(); }
		catch(Exception e)
		{
			throw new UnavailableException(this,
				"Error reading address file: "+e);
		}
	}

	protected void doGet(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		res.setContentType("text/html");
		res.setHeader("pragma", "no-cache");
		PrintWriter out = res.getWriter();
		out.print("<HTML><HEAD><TITLE>List Manager</TITLE></HEAD>");
		out.print("<BODY><H3>Members:</H3><UL>");
		for(int i=0; i<addresses.size(); i++)
			out.print("<LI>" + addresses.elementAt(i));
		out.print("</UL><HR><FORM METHOD=POST>");
		out.print("Enter your email address: <INPUT TYPE=TEXT NAME=email><BR>");
		out.print("<INPUT TYPE=SUBMIT NAME=action VALUE=subscribe>");
		out.print("<INPUT TYPE=SUBMIT NAME=action VALUE=unsubscribe>");
		out.print("</FORM></BODY></HTML>");
       		out.close();
     	}
 
    	protected void doPost(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		String email = req.getParameter("email");
		String msg;
		if(email == null)
		{
        			res.sendError(res.SC_BAD_REQUEST,
                      		"No email address specified.");
        			return;
      		}
		if(req.getParameter("action").equals("subscribe"))
		{
			if(subscribe(email))
				msg = "Address " + email + " has been subscribed.";
			else
			{
				res.sendError(res.SC_BAD_REQUEST,
					"Address " + email + " was already subscribed.");
           			return;
        			}
      		}
      		else
      		{
        			if(unsubscribe(email))
          			msg = "Address " + email + " has been removed.";
       			else
        			{
          			res.sendError(res.SC_BAD_REQUEST,
                        			"Address " + email + " was not subscribed.");
          			return;
        			}
      		}

		res.setContentType("text/html");
      		res.setHeader("pragma", "no-cache");
      		PrintWriter out = res.getWriter();
      		out.print("<HTML><HEAD><TITLE>List Manager</TITLE></HEAD><BODY>");
      		out.print(msg);
      		out.print("<HR><A HREF=\"");
      		out.print(req.getRequestURI());
      		out.print("\">Show the list</A></BODY></HTML>");
      		out.close();
    	}

    	public String getServletInfo()
    	{
      		return "ListManagerServlet 1.0 by Stefan Zeiger";
    	}

    	private synchronized boolean subscribe(String email) throws IOException
    	{
      		if(addresses.contains(email)) return false;
      		addresses.addElement(email);
      		save();
      		return true;
    	}

    	private synchronized boolean unsubscribe(String email) throws IOException
    	{
      	if(!addresses.removeElement(email)) return false;
     	save();
      	return true;
    	}

    	private void save() throws IOException
    	{
	ObjectOutputStream out =
	new ObjectOutputStream(new FileOutputStream(filename));
	out.writeObject(addresses);
	out.close();
	}
}
*/