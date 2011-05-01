<%@ page language="java" %>

<%
	String whereTo = request.getParameter("resource_type");

	if (whereTo.equals("url"))
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/classtm/AddUrlResource.jsp" + "?" + request.getQueryString());
		dispatcher.forward(request, response);
	}
	else if (whereTo.equals("text"))
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/classtm/AddTextResource.jsp" + "?" + request.getQueryString());
		dispatcher.forward(request, response);
	}
	else if (whereTo.equals("file"))
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/classtm/AddFileResource.jsp" + "?" + request.getQueryString());
		dispatcher.forward(request, response);
	}
	else
	{
		out.print("<h2>Resource type unknown.<h2/><br />");
	}
%>