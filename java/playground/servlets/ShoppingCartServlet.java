import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class ShoppingCartServlet extends HttpServlet
{
	protected void doGet(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		res.setContentType("text/html");
		PrintWriter out = res.getWriter();
		out.print("<HTML>"+
			 "<HEAD>"+
			 "<TITLE>Bogdan\'s Shopping Cart</TITLE>"+
			 "</HEAD>"+
			 "<BODY>"+
			 "<CENTER>"+
			 "<H1>Welcome to Bogdan\'s shop.<BR>"+
			 "Feel free to make as many purchases as you like...<BR>"+
			 "Thank you<H1><BR><BR>"+
			 "<FORM METHOD=POST>"+
			 "<INPUT TYPE=SUBMIT NAME=foo VALUE=\"Put a FOO in the shopping cart\">"+
			 "<BR>"+
			 "<INPUT TYPE=SUBMIT NAME=bar VALUE=\"Put a BAR in the shopping cart\">"+
			 "<BR>"+
			 "<INPUT TYPE=SUBMIT NAME=see VALUE=\"See the shopping cart\'s contents\">"+
			 "<BR>"+
			 "<INPUT TYPE=SUBMIT NAME=buy VALUE=\"Buy what is in the shopping cart\">"+
			 "</FORM>"+
			 "</BODY>"+
			 "</HTML>");
		out.close();
	}
	
	protected void doPost(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		String msg;
		
		HttpSession session = req.getSession(true);
		if (session.isNew())
		{
			session.putValue("foo", new int[] {0});
			session.putValue("bar", new int[] {0});
		}
		
		int[] foo = (int[]) session.getValue("foo");
		int[] bar = (int[]) session.getValue("bar");
		
		if (req.getParameter("foo") != null)
		{
			foo[0]++;
			msg = "You added a FOO to your shopping cart.";
			msg += " You now have " + foo[0] + ".";
		}
		else if (req.getParameter("bar") != null)
		{
			bar[0]++;
			msg = "You added a BAR to your shopping cart.";
			msg += " You now have " + bar[0] + ".";
		}
		else if (req.getParameter("see") != null)
		{
			msg = "You have " + foo[0] + " FOO";
			if (foo[0] != 1)
				msg += "s";
			msg += " and " + bar[0] + " BAR";
			if (bar[0] != 1)
				msg += "s";
			msg += " in your shopping cart.";
		}
		else
		{
			session.invalidate();
			msg = "Your order for " + foo[0] + " FOO";
			if (foo[0] != 1)
				msg += "s";
			msg += " and " + bar[0] + " BAR";
			if (bar[0] != 1)
				msg += "s";
			msg += " has been accepted. Your shopping cart is now empty.";
		}
		
		res.setContentType("text/html");
		res.setHeader("pragma", "no-cache");
		PrintWriter out = res.getWriter();
		out.print("<HTML>"+
			 "<HEAD>"+
			 "<TITLE>Bogdan\'s Shopping Cart</TITLE>"+
			 "</HEAD>"+
			 "<BODY>"+
			 msg+
			 "<HR>"+
			 "<A HREF=\""+
			 req.getRequestURI()+
			 "\">Back to the shop</A>"+
			 "</BODY>"+
			 "</HTML>");
		out.close();
	}
}