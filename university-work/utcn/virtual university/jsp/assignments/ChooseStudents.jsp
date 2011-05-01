<%@ page language="java" import="java.sql.*, ro.utcluj.vu.assignments.AssignmentData" %>

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
		if (!rs.next())
		{
%>
<h2>No students available in the class.</h2>
<%
		}
		else
		{
%>

<h1>Step 2: choose student(s)</h1>

<br />
<font color="red">Tip:</font> If you choose more than one student, the task you have<br />
previously selected will be assigned to each of them separately.<br />
This will have the same overall effect as if you repeat the<br />
assignemnt process for each of them in particular.
<br />
<br />

<table class="bgcolor" cellspacing="0" cellpadding="0">
<form action="assTask3.do" method="post">
<input type="hidden" name="task" value="<%= data.getTaskIdx() %>" />

<%
			int i = 0;
			do
			{
%>
<tr class="bgcolor">
<%
				if (++i % 2 == 0)
				{
%>
<td class="bgcolor2" align="right">
<%
				}
				else
				{
%>
<td class="bgcolor3" align="right">
<%
				}
%>
<input	type="checkbox"
	name="students"
	value="<%= rs.getString("student_ID") %>"/>
</td>
<%
				if (i % 2 == 0)
				{
%>
<td class="bgcolor2" align="left" style="padding-left: 10px; padding-right: 100px">
<%
				}
				else
				{
%>
<td class="bgcolor3" align="left" style="padding-left: 10px; padding-right: 100px">
<%
				}
%>
<nobr>
<%= rs.getString("first_name") %> <%= rs.getString("last_name") %>
</nobr>
</td>

</tr>

<%
			}
			while (rs.next());
		}
	}
%>

<tr>
<td colspan="2" align="right">
<input class="custom" type="submit" value=" Next >> " />
</td>
</tr>

</form>

</td>
</table>

</body>

</html>