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
	String grade = "";
	String gradeComments = "";

	if (data != null)
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			grade = rs.getString("grade");
			gradeComments = rs.getString("grade_comments");
		}
		else
		{
			test = true;
		}
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
%>
<h1>
Grading information
</h1>

<table class="bgcolor" width="70%">

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Grade:
</td>
<td class="bgcolor2" align="left">
<%= grade %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Grade comments:
</td>
<td class="bgcolor2" align="left">
<%= gradeComments %>
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