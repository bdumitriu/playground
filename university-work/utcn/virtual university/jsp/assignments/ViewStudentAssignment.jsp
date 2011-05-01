<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 ro.utcluj.vu.tasks.TaskData,
				 java.sql.ResultSet,
				 java.util.ArrayList"%>

<%
	Object tmp = session.getAttribute("data");
	TaskData data = null;
	if ((tmp != null) && (tmp instanceof TaskData))
	{
		data = (TaskData) tmp;
	}

	boolean test = false;
	String name = "";
	String comments = "";
	String description = "";
	String difficulty = "";
	int idx = -1;
	String classID = "";
	ArrayList files = null;

	if (data != null)
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			name = rs.getString("title");
			comments = rs.getString("teacher_notes");
			description = rs.getString("description");
			difficulty = rs.getString("difficulty");
			idx = rs.getInt("idx");
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
No information found in the database for the requested task.
</h1>

<%
	}
	else
	{
%>

<h1>
Task details
</h1>

<table class="bgcolor" width="70%">

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Task name:
</td>
<td class="bgcolor2" align="left">
<%= name %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Comments from teacher:
</td>
<td class="bgcolor2" align="left">
<%= comments %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
General task description:
</td>
<td class="bgcolor2" align="left">
<%= description %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Task difficulty:
</td>
<td class="bgcolor2" align="left">
<%= difficulty %>
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
<a href="../../data/classes/<%= classID %>/tasks/<%= idx %>/<%= files.get(i) %>"
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
<a
	href="studListAss.do"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
BACK
</a>

<%
	}
%>

</td>
</table>

</body>

</html>