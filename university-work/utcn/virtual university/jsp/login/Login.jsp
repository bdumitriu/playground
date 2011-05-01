<%@ page language="java" import="ro.utcluj.vu.login.LoginData, java.sql.ResultSet,
				 ro.utcluj.vu.main.IdentificationInfo,
				 ro.utcluj.vu.main.SecurityChecker" %>

<%
	LoginData data = (LoginData) session.getAttribute("data");
	ResultSet rs = data.getResult();

	boolean loginOk = false;

	while (rs.next())
	{
		String pass = rs.getString("password");

		loginOk = true;

		if (pass.equals(data.getPassword()))
		{
			// checking the user role
			String role = rs.getString("description");

			// forward the user standard page
			if (role.equals("user"))
			{
				String userID = rs.getString("user_ID");
				IdentificationInfo id = new IdentificationInfo();

				id.setUserID(userID);
				session.setAttribute("id", id);

				SecurityChecker secCheck = (SecurityChecker)
					getServletConfig().getServletContext().getAttribute("secCheck");
				secCheck.userInfoUpdate(session);

				RequestDispatcher dispatcher = getServletConfig().
					getServletContext().getRequestDispatcher("/classChoice.do");
				dispatcher.forward(request, response);
			}

			// forward the admin page
			else if (role.equals("admin"))
			{
			}

			// forward the visitor's page
			else if (role.equals("visitor"))
			{
			}

		}
		else
		{
			loginOk = false;
		}
	}

	if (loginOk == false)
	{
		RequestDispatcher dispatcher = getServletConfig().
			getServletContext().getRequestDispatcher("/jsp/login/LoginFailed.jsp");
		dispatcher.forward(request, response);
	}
%>
