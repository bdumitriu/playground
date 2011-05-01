<%@ page language="java" import="java.sql.*, ro.utcluj.vu.classtm.ClassTMData,
				 ro.utcluj.vu.main.IdentificationInfo"%>

<%
	ClassTMData data = (ClassTMData) session.getAttribute("data");
	ResultSet rs = data.getResult();

	Object tmp = session.getAttribute("id");
	IdentificationInfo id = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		id = (IdentificationInfo) tmp;

%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	if (!rs.next())
	{
%>

<h2>No resource information available at this time.</h2>


<%
	}
	else
	{
		if (rs != null)
		{
			session.setAttribute("data_redirected", data);
			String resourceType = rs.getString("description");

			if (resourceType.equals("url"))
			{
				RequestDispatcher dispatcher =
					getServletConfig().getServletContext().
					getRequestDispatcher("/jsp/classtm/EditURLResource.jsp");
				dispatcher.forward(request, response);
			}
			else if (resourceType.equals("txt"))
			{
				RequestDispatcher dispatcher =
					getServletConfig().getServletContext().
					getRequestDispatcher("/jsp/classtm/EditTXTResource.jsp");
				dispatcher.forward(request, response);
			}
			else if (resourceType.equals("fil"))
			{
				RequestDispatcher dispatcher =
					getServletConfig().getServletContext().
					getRequestDispatcher("/jsp/classtm/EditFILEResource.jsp");
				dispatcher.forward(request, response);
			}
			else
%>
<h2>Resource type unknown.</h2>
<%
		}
	}
	}
%>
</table>

</body>

</html>