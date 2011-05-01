<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.announcements.AnnouncementData, ro.utcluj.vu.utils.Tools" %>

<%
	AnnouncementData data = (AnnouncementData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">


<%
	ResultSet rs = data.getResult();

	if (!rs.next())
	{
%>

<h2>No announcements available at this time.</h2>


<%
	}
	else
	{
		if (rs != null)
		{
%>
<h1>Announcements</h1>
<%
			do
			{
				java.util.Date creationDate = rs.getDate("creation_date");
				GregorianCalendar calendar  = new GregorianCalendar();
				calendar.setTime(creationDate);
%>
<b>
<%= Tools.printDateTime(calendar) %>
</b>

<br />
<%= rs.getString("announcement") %>
<br /><br />

<%
			} while (rs.next());
		}
	}
%>

</td>
</table>

</body>

</html>