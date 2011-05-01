import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class HelloClientServlet extends HttpServlet
{
	protected void doGet(HttpServletRequest req, 
			   HttpServletResponse res)
		throws ServletException, IOException
	{
		res.setContentType("text/html");
		PrintWriter out = res.getWriter();
		out.println("<HTML><HEAD><TITLE>Test Page</TITLE>"+
			"</HEAD><BODY><H1>Hello CLIENT!</H1>"+
			"<CENTER>by Bogdan DUMITRIU</BODY>");
		out.close();
	}
	
	public String getServletInfo()
	{
		return "Hello CLIENT by Bogdan DUMITRIU";
	}
}