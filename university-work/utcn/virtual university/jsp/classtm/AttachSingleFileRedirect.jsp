<%@ page language="java" %>

<%
	if (request.getParameter("attach") != null)
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/classtm/attSingleFile.do?" + request.getQueryString());
		dispatcher.forward(request, response);
	}
	else
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher(request.getParameter("submitAction") + "?" + request.getQueryString());
		dispatcher.forward(request, response);
	}
%>