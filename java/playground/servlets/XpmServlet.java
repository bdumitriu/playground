
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class XpmServlet extends HttpServlet
{
 	protected void doGet(HttpServletRequest req, HttpServletResponse res)
		throws ServletException, IOException
	{
		String color = req.getParameter("color");
		if (color == null) 
			color = "FFFFFF";

		res.setContentType("image/x-xpixmap");
		res.setHeader("pragma", "no-cache");

		ServletOutputStream out = res.getOutputStream();
		out.print("/* XPM */\n"+
		"static char * hello_xpm[] = {\n"+
		"\"17 13 2 1\",\n"+
		"\" \tc #000000\",\n"+
		"\"X\tc #"+color+"\",\n"+
		"\"XXXXX       XXXXX\",\n"+
		"\" XXXXX     XXXXX \",\n"+
		"\"  XXXXX   XXXXX  \",\n"+
		"\"   XXXXX XXXXX   \",\n"+
		"\"    XXXXXXXXX    \",\n"+
		"\"     XXXXXXX     \",\n"+
		"\"      XXXXX      \",\n"+
		"\"     XXXXXXX     \",\n"+
		"\"    XXXXXXXXX    \",\n"+
		"\"   XXXXX XXXXX   \",\n"+
		"\"  XXXXX   XXXXX  \",\n"+
		"\" XXXXX     XXXXX \",\n"+
		"\"XXXXX       XXXXX\"};\n");
		out.close();
	}
}