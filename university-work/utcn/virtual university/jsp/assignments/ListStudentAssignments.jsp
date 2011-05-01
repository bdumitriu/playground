<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.assignments.AssignmentData,
				 ro.utcluj.vu.utils.Tools,
				 ro.utcluj.vu.utils.ValueChecker" %>

<%
	AssignmentData data = (AssignmentData) session.getAttribute("data");
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
<h2>No tasks assigned at this time.</h2>
<%
		}
		else
		{
%>

<h1>List of assigned tasks</h1>

<table>

<tr>
<td colspan="4">
</td>
<td colspan="3" align="left" style="padding-left: 7px">
*** - can be submitted after due date
</td>
</tr>

<tr>
<td class="header">Nr.</td>
<td class="header">Task name</td>
<td class="header">Assigned on</td>
<td class="header">Due on</td>
<td class="header">***</td>
<td class="header">Status</td>
<td class="header">Action to take</td>
</tr>

<%
			do
			{
%>

<tr>
<td class="content"><%= i++ %></td>
<td class="content"><%= rs.getString("title") %></td>
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
				int status = rs.getInt("status");
				switch (status)
				{
					case 1:
					{
						out.print("<td class=\"content\">unsubmitted</td>");
						break;
					}
					case 2:
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
						out.print("<td class=\"content\">submitted<br />");
						out.print("on " + str);
						out.print("</td>");
						break;
					}
					case 3:
					{
						out.print("<td class=\"content\">read by teacher</td>");
						break;
					}
					case 4:
					{
						out.print("<td class=\"content\">graded with <b>");
						out.print(rs.getString("grade"));
						out.print("</b>");
						if ((rs.getString("grade_comments") != null) &&
							(!rs.getString("grade_comments").equals("")))
						{
							out.print("<br />(commented)");
						}
						out.print("</td>");
						break;
					}
				}
%>

<td class="content">
<a
	href="studViewAss.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('View task details'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>view task details</nobr>
</a>
<br />

<%
				cal.setTimeInMillis(rs.getDate("due_on").getTime());
				// make sure student can submit until midnight
				cal.set(GregorianCalendar.HOUR_OF_DAY, 23);
				cal.set(GregorianCalendar.MINUTE, 59);
				cal.set(GregorianCalendar.SECOND, 59);
				cal.set(GregorianCalendar.MILLISECOND, 999);
				GregorianCalendar today = new GregorianCalendar();
				today.setTimeInMillis(System.currentTimeMillis());
				if ((ValueChecker.valueBetween(today, null, cal)) || (rs.getBoolean("allow_late_submission")))
				{
					if (status == 1)
					{
%>
<a
	href="../../jsp/assignments/SubmitAnswerPage.jsp?idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Submit answer'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>submit answer</nobr>
</a>
<br />
<%
					}
					else
					{
%>
<a
	href="readAnswer.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Read your answer'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>read your answer</nobr>
</a>
<br />
<%
					}
				}
				else
				{
					if (status == 1)
					{
%>
<font color="red"><nobr>due date expired<nobr></font>
<br />
<%
					}
					else
					{
%>
<a
	href="readAnswer.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Read your answer'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>read your answer</nobr>
</a>
<br />
<%
					}
				}
				if ((rs.getString("grade_comments") != null) &&
					(!rs.getString("grade_comments").equals("")))
				{
%>
<a
	href="readCom.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
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