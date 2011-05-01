<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.login.ClassChoiceData" %>

<%
	ClassChoiceData data = (ClassChoiceData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table width="100%" height="100%">

<td align="center" valign="center">

<table>

<%
	ResultSet rs = data.getResult();

	if (!rs.next())
	{
%>
<caption>
<h2>No classes available at this tinme.</h2>
</caption>
<%
	}
	else
	{
		rs.previous();
%>

<caption>
<h1>List of available classes</h1>
</caption>

<tr>
<td class="header">Course title</td>
<td class="header">Class name</td>
<td class="header">Position in class</td>
</tr>

<%
	if (rs != null)
	{
		boolean noCourses = true;
		while (rs.next())
		{
			noCourses = false;
%>

<tr>
<td class="content">
<%= rs.getString("course_name") %>
</td>

<td class="content">
<%= rs.getString("class_name") %>
</td>

<td class="content">
<%= rs.getString("position") %>
</td>

<td class="content">
<a
	href="displayMenu.do?class_ID=<%= rs.getString("class_ID") %>&position=<%= rs.getString("position") %>"
	onMouseOver="windowStatus('Enter class'); return true;"
	onMouseOut="windowStatus(''); return true;">
enter<br />class
</a>
</td>
</tr>

<%
		}
	}
	}
%>
</table>

</td>

</table>

</body>

</html>