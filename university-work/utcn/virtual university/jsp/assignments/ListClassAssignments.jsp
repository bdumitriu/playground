<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.assignments.AssignmentData,
				 ro.utcluj.vu.utils.Tools,
				 ro.utcluj.vu.utils.ValueChecker" %>

<%
	AssignmentData data = (AssignmentData) session.getAttribute("data");

	int status = data.getStatus();
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
<h2>No task in this category at this time.</h2>
<%
		}
		else
		{
			switch (status)
			{
				case 1:
				{
%>
<h1>List of unsubmitted tasks</h1>
<%
					break;
				}
				case 2:
				{
%>
<h1>List of submitted tasks</h1>
<%
					break;
				}
				case 3:
				{
%>
<h1>List of read tasks</h1>
<%
					break;
				}
				case 4:
				{
%>
<h1>List of graded tasks</h1>
<%
					break;
				}
				default:
				{
%>
<h1>Value <%= status %> is not valid for status parameter.</h1>
<%
					break;
				}
			}
%>

<table>

<%
			if (status == 1)
			{
%>
<tr>
<td colspan="5">
</td>
<td colspan="3" align="left" style="padding-left: 7px">
*** - can be submitted after due date
</td>
</tr>
<%
			}
%>

<tr>
<td class="header">Nr.</td>
<td class="header">Task name</td>
<td class="header">Assigned to</td>
<td class="header">Assigned on</td>
<td class="header">Due on</td>
<%
			if (status > 1)
			{
%>
<td class="header">Submitted on</td>
<%
			}
			if (status == 1)
			{
%>
<td class="header">***</td>
<%
			}
			if (status == 4)
			{
%>
<td class="header">Grade</td>
<%
			}
%>
<td class="header">Action to take</td>
</tr>

<%
			do
			{
%>

<tr>
<td class="content"><%= i++ %></td>
<td class="content"><%= rs.getString("title") %></td>
<td class="content"><%= rs.getString("first_name") %> <%= rs.getString("last_name") %></td>
<%
				java.sql.Date date = rs.getDate("assigned_on");
				GregorianCalendar cal = new GregorianCalendar();
				String str;
				if (date != null)
				{
					cal.setTimeInMillis(date.getTime());
					str = Tools.printDate(cal);
				}
				else
				{
					str = "&lt;not known&gt;";
				}
%>
<td class="content"><%= str %></td>
<%
				date = rs.getDate("due_on");
				if (date != null)
				{
					cal.setTimeInMillis(date.getTime());
					str = Tools.printDate(cal);
				}
				else
				{
					str = "&lt;not known&gt;";
				}
%>
<td class="content"><%= str %></td>
<%
				if (status > 1)
				{
					date = rs.getDate("submitted_on");
					if (date != null)
					{
						cal.setTimeInMillis(date.getTime());
						str = Tools.printDate(cal);
					}
					else
					{
						str = "&lt;not known&gt;";
					}
%>
<td class="content"><%= str %></td>
<%
				}
				if (status == 1)
				{
					if (rs.getBoolean("allow_late_submission"))
					{
%>
<td class="content">yes</td>
<%
					}
					else
					{
%>
<td class="content">no</td>
<%
					}
				}
				if (status == 4)
				{
%>
<td class="content"><b><%= rs.getString("grade") %></b>
<%
						if ((rs.getString("grade_comments") != null) &&
							(!rs.getString("grade_comments").equals("")))
						{
%>
<br />(commented)
<%
						}
%>
</td>
<%
				}
%>

<td class="content">
<a
	href="tchViewAss.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>&status=<%= status %>"
	onMouseOver="windowStatus('View task details and comments (both to student and yourself)'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>view task details</nobr>
</a>
<br />
<a
	href="delAss.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>&status=<%= status %>"
	onMouseOver="windowStatus('Delete assignment'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>delete assignment</nobr>
</a>
<br />
<%
				if (status > 1)
				{
%>
<a
	href="readAnswer.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>&status=<%= status %>"
	onMouseOver="windowStatus('Read student answer'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>read student answer</nobr>
</a>
<br />
<%
				}
				if ((status == 2) || (status == 3))
				{
%>
<a
	href="../../jsp/assignments/GradeAssignmentPage.jsp?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>&status=<%= status %>"
	onMouseOver="windowStatus('Grade assignment'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>grade assignment</nobr>
</a>
<br />
<%
				}
				if ((rs.getString("grade_comments") != null) &&
					(!rs.getString("grade_comments").equals("")))
				{
%>
<a
	href="readCom.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>&status=<%= status %>"
	onMouseOver="windowStatus('Read grade comments'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>read grade comments</nobr>
</a>
<br />
<%
				}
%>
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