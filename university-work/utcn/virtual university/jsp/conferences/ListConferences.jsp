<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.conferences.ConferenceData,
				 ro.utcluj.vu.utils.Tools" %>

<%
	ConferenceData data = (ConferenceData) session.getAttribute("data");
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
		int i = 1;
		if (!rs.next())
		{
%>
<h2>No conferences available at this time.</h2>
<%
		}
		else
		{
%>

<h1>List of conferences</h1>

<table>

<tr>
<td class="header">Nr.</td>
<td class="header">Conference name</td>
<td class="header">Conference visibility</td>
<td class="header">Students can create<br />main topics</td>
<td class="header">Students can<br />post answers</td>
</tr>

<%
			do
			{
%>

<tr>
<td class="content"><%= i++ %></td>
<td class="content"><%= rs.getString("name") %></td>
<%
				byte vis = rs.getByte("visibility");
				if (vis == 0)
				{
%>
<td class="content">is visible</td>
<%
				}
				else if (vis == 1)
				{
%>
<td class="content">is not visible</td>
<%
				}
				else
				{
%>
<td class="content">is visible from<br />
<%
					java.sql.Date date = rs.getDate("start_date");
					GregorianCalendar cal = new GregorianCalendar();
					cal.setTime(date);
					out.print(Tools.printDate(cal));
%>
</td>
<%
				}

				byte stInt = rs.getByte("student_interactivity");
				if (stInt == 0)
				{
%>
<td class="content"><input type="checkbox" checked disabled></td>
<td class="content"><input type="checkbox" checked disabled></td>
<%
				}
				else if (stInt == 1)
				{
%>
<td class="content"><input type="checkbox" disabled></td>
<td class="content"><input type="checkbox" checked disabled></td>
<%
				}
				else if (stInt == 2)
				{
%>
<td class="content"><input type="checkbox" checked disabled></td>
<td class="content"><input type="checkbox" disabled></td>
<%
				}
				else
				{
%>
<td class="content"><input type="checkbox" disabled></td>
<td class="content"><input type="checkbox" disabled></td>
<%
				}
%>

<td class="content">
<a
	href="editConf1.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Edit conference'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />conference
</a>
</td>

<td class="content">
<a
	href="delConf.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Delete conference'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />conference
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

<br /><br />
<a
	href="../../jsp/conferences/AddConferencePage.jsp"
	onMouseOver="windowStatus('Add conference'); return true;"
	onMouseOut="windowStatus(''); return true;">
Add conference
</a>
&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
<a
	href="../../html/conferences/reorderConf1.do"
	onMouseOver="windowStatus('Reorder conferences'); return true;"
	onMouseOut="windowStatus(''); return true;">
Reorder conferences
</a>

</td>
</table>

</body>

</html>