<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 java.io.File,
				 ro.utcluj.vu.conferences.ConferenceData,
				 java.sql.ResultSet,
				 java.util.Map,
				 java.util.ArrayList"%>

<%
	Object tmp = session.getAttribute("data");
	ConferenceData data = null;
	if ((tmp != null) && (tmp instanceof ConferenceData))
	{
		data = (ConferenceData) tmp;
	}

	boolean test = false;
	String name = "";
	String description = "";
	byte studentInteractivity = -1;
	String idx = "";
	String classID = "";
	ArrayList files = null;

	if (data != null)
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			name = rs.getString("name");
			description = rs.getString("description");
			studentInteractivity = rs.getByte("student_interactivity");
			idx = (String) rs.getString("idx");
			classID = (String) rs.getString("class_id");
			files = (ArrayList) data.getFilesMap().get(new Integer(data.getIndex()));
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
No information found in the database for the requested conference.
</h1>

<%
	}
	else
	{
%>

<h1>
Conference details
</h1>

<table class="bgcolor" width="70%">

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Conference name:
</td>
<td class="bgcolor2" align="left">
<%= name %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Conference description:
</td>
<td class="bgcolor2" align="left">
<%= description %>
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
<a href="../../data/classes/<%= classID %>/conferences/<%= idx %>/<%= files.get(i) %>"
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
		IdentificationInfo id = null;
		tmp = session.getAttribute("id");

		if ((tmp != null) && (tmp instanceof IdentificationInfo))
		{
			id = (IdentificationInfo) tmp;
		}
		else
		{
			id = new IdentificationInfo(null, null, "");
		}

		if ((id.getPosition().equals("teacher")) || (studentInteractivity == 0) ||
			(studentInteractivity == 2))
		{
%>
<a
	href="../../jsp/conferences/AddMainTopicData.jsp?idx=<%= data.getIndex() %>"
	onMouseOver="windowStatus('Add main topic'); return true;"
	onMouseOut="windowStatus(''); return true;">
Add main topic
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