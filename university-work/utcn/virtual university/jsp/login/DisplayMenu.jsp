<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo" %>

<%
	IdentificationInfo id = (IdentificationInfo) session.getAttribute("id");

	if (id.getPosition().equals("teacher"))
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/login/TeacherMenu.jsp");
		dispatcher.forward(request, response);
	}
	else
	{
		RequestDispatcher dispatcher = getServletConfig().getServletContext().
			getRequestDispatcher("/jsp/login/StudentMenu.jsp");
		dispatcher.forward(request, response);
	}
%>