<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 ro.utcluj.vu.assignments.AssignmentData,
				 java.sql.ResultSet,
				 java.util.ArrayList,
				 ro.utcluj.vu.utils.Tools,
				 java.util.GregorianCalendar"%>

<%
	Object tmp = session.getAttribute("data");
	AssignmentData data = null;
	if ((tmp != null) && (tmp instanceof AssignmentData))
	{
		data = (AssignmentData) tmp;
	}
	tmp = session.getAttribute("id");
	IdentificationInfo id;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		id = (IdentificationInfo) tmp;
	}
	else
	{
		id = new IdentificationInfo("", "", "user");
	}

	boolean test = false;
	String answer = "";
	java.sql.Date ass_on = null;
	java.sql.Date due_on = null;
	java.sql.Date submitted_on = null;
	int idx = -1;
	int status = -1;
	String classID = "";
	ArrayList files = null;

	if (data != null)
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			answer = rs.getString("answer");
			ass_on = rs.getDate("assigned_on");
			due_on = rs.getDate("due_on");
			submitted_on = rs.getDate("submitted_on");
			idx = rs.getInt("idx");
			status = rs.getInt("status");
			classID = (String) rs.getString("class_id");
			files = (ArrayList) data.getFilesMap().get((new Integer(idx)));
		}
		else
		{
			test = true;
		}
	}

	int nrFiles = 0;
	if (files != null)
	{
		nrFiles = files.size();
	}
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	if (test)
	{
%>

<h1>
No information found in the database for the requested assignment.
</h1>

<%
	}
	else
	{
		if (id.getPosition().equals("teacher"))
		{
%>
<h1>
Student's answer
</h1>
<%
		}
		else if (id.getPosition().equals("student"))
		{
%>
<h1>
Your answer
</h1>
<%
		}
%>
<table class="bgcolor" width="70%">

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Answer:
</td>
<td class="bgcolor2" align="left">
<pre><%= Tools.replaceHTMLChars(answer) %></pre>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Assigned on:
</td>
<td class="bgcolor2" align="left">
<%
	GregorianCalendar cal = new GregorianCalendar();
	cal.setTimeInMillis(ass_on.getTime());
%>
<%= Tools.printDate(cal) %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Was due on:
</td>
<td class="bgcolor2" align="left">
<%
	cal.setTimeInMillis(due_on.getTime());
%>
<%= Tools.printDate(cal) %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Submitted on:
</td>
<td class="bgcolor2" align="left">
<%
	cal.setTimeInMillis(submitted_on.getTime());
%>
<%= Tools.printDate(cal) %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Attached files:
<input type="hidden" name="files" value="<%= nrFiles %>" />
</td>
<td class="bgcolor2" align="left">
<%
	if (nrFiles == 0)
	{
%>
none
<%
	}
	else
	{
%>
<table border="0">
<%
		for (int i = 0; i < nrFiles; i++)
		{
			out.print("<tr><td valign=\"bottom\">");
			out.print("<img src=\"../../img/file.jpg\">");
			out.print("</td><td valign=\"bottom\" style=\"padding-left: 3px\">");
%>
<a href="../../data/classes/<%= classID %>/assignments/<%= idx %>/<%= files.get(i) %>"
	onMouseOver="windowStatus('Right click and choose Save Target As...'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%
			out.print(files.get(i));
			out.print("</a></td></tr>");
		}
%>
</table>
<%
	}
%>
</td>
</tr>

</table>

<br /><br />

<%
		if (id.getPosition().equals("teacher"))
		{
%>
<a
	href="tchListAss.do?status=<%= request.getParameter("status") %>"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>BACK</nobr>
</a>
<%
			if (status == 2)
			{
%>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a
	href="markAsRead.do?class_ID=<%= classID %>&idx=<%= idx %>&status=<%= request.getParameter("status") %>"
	onMouseOver="windowStatus('Mark answer as read'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>MARK ANSWER AS READ</nobr>
</a>
<%
			}
		}
		else if (id.getPosition().equals("student"))
		{
%>
<a
	href="studListAss.do"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
BACK
</a>
<%
		}
%>

<%
	}
%>

</td>
</table>

</body>

</html>