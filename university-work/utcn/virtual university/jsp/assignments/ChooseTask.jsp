<%@ page language="java" import="java.sql.*, ro.utcluj.vu.tasks.TaskData" %>

<%
	TaskData data = (TaskData) session.getAttribute("data");
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
<h2>No tasks available at this time. To create tasks use the Task management menu.</h2>
<%
		}
		else
		{
%>

<h1>Step 1: choose task to assign</h1>

<table class="bgcolor" cellspacing="0" cellpadding="0">
<form action="assTask2.do" method="post">
<%
			int i = 1;
			do
			{
%>

<tr class="bgcolor">
<%
				if (i % 2 == 0)
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
				if (i == 1)
				{
%>
<input type="radio" name="task" checked value="<%= rs.getInt("idx") %>">
<%
				}
				else
				{
%>
<input type="radio" name="task" value="<%= rs.getInt("idx") %>">
<%
				}
%>
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
<%= rs.getString("title") %> (<%= rs.getString("difficulty") %>)
</nobr>
</td>

<%
				if (i % 2 == 0)
				{
%>
<td class="bgcolor2" align="left" style="padding-left: 100px; padding-right: 10px">
<%
				}
				else
				{
%>
<td class="bgcolor3" align="left" style="padding-left: 100px; padding-right: 10px">
<%
				}
%>
<a	href="viewTask.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('View task'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>view task</nobr>
</a>
</td>
</tr>

<%
				i++;
			}
			while (rs.next());
%>

<tr><td colspan="3" align="right"><input class="custom" type="submit" value=" Next >> " /></td></tr>

</form>
</td>
</table>
<%
		}
	}
%>


</td>
</table>

</body>

</html>