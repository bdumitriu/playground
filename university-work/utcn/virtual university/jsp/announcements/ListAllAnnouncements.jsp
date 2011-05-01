<%@ page language="java" import="java.sql.*, java.util.GregorianCalendar, ro.utcluj.vu.announcements.AnnouncementData, ro.utcluj.vu.utils.*" %>

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

	if (rs != null)
	{
		if (!rs.next())
		{
%>
<h2>No announcements available at this time.</h2>
<%
		}
		else
		{
%>

<h1>List of announcements</h1>

<table>

<tr>
<td class="header">Announcement</td>
<td class="header">Announcement visibility</td>
<td class="header">Creation date</td>
</tr>

<%
			do
			{
				String ann = rs.getString("announcement");

				if (ann.length() > 20)
				{
					ann = ann.substring(0, 20);
				}
%>

<tr>
<td class="content"><%= ann %>...</td>
<%
				java.util.Date current = new java.util.Date(System.currentTimeMillis());
				java.util.Date startDate = rs.getDate("start_date");
				java.util.Date endDate = rs.getDate("end_date");

				boolean visible = ValueChecker.valueBetween(current, startDate, endDate);

				if (visible)
				{
%>
<td class="content"><b>currently visible</b>
<%
				}
				else
				{
%>
<td class="content"><b>currently not visible</b>
<%
				}
				if ((startDate != null) || (endDate != null))
				{
%>
<%
					if (startDate != null)
					{
						GregorianCalendar sdcalendar = new GregorianCalendar();
						sdcalendar.setTime(startDate);
%>
<br />&lt from <%= Tools.printDate(sdcalendar) %> &gt
<%
					}
					if (endDate != null)
					{
						GregorianCalendar edcalendar = new GregorianCalendar();
						edcalendar.setTime(endDate);
%>
<br />&lt until <%= Tools.printDate(edcalendar) %> &gt
<%
					}
%>
<%
				}
%>
</td>
<td class="content">

<%
		java.util.Date creationDate = rs.getDate("creation_date");
		GregorianCalendar calendar = new GregorianCalendar();
		calendar.setTime(creationDate);
%>
<%= Tools.printSimpleDateTime(calendar) %>
</td>

<td class="content">
<a
	href="editAnn1.do?announcement_ID=<%= rs.getString("announcement_ID") %>"
	onMouseOver="windowStatus('Edit announcement'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />announcement
</a>
</td>

<td class="content">
<a
	href="delAnn.do?announcement_ID=<%= rs.getString("announcement_ID") %>"
	onMouseOver="windowStatus('Delete announcement'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />announcement
</a>
</td>

</tr>

<%
			}
			while (rs.next());
%>
</table>
<%
		}
	}
%>

</td>
</table>

</body>

</html>