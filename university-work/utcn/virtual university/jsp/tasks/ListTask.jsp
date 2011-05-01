<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.tasks.TaskData" %>

<%
	TaskData data = (TaskData) session.getAttribute("data");
	Map files = data.getFilesMap();
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
<h2>No tasks available at this time.</h2>
<%
		}
		else
		{
%>

<h1>List of tasks</h1>

<table>

<tr>
<td class="header">Nr.</td>
<td class="header">Task name</td>
<td class="header">Task difficulty</td>
<td class="header">Nr. of attched files</td>
</tr>

<%
			do
			{
%>

<tr>
<td class="content"><%= i++ %></td>
<td class="content"><%= rs.getString("title") %></td>
<td class="content"><%= rs.getString("difficulty") %></td>
<%
				Integer key = new Integer(rs.getInt("idx"));
				ArrayList value = (ArrayList) files.get(key);
				if (value != null)
				{
%>
<td class="content"><%= value.size() %></td>
<%
				}
				else
				{
%>
<td class="content">none</td>
<%
				}
%>

<td class="content">
<a
	href="viewTask.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('View task details'); return true;"
	onMouseOut="windowStatus(''); return true;">
view task<br />details
</a>
</td>

<td class="content">
<a
	href="editTask1.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Edit task'); return true;"
	onMouseOut="windowStatus(''); return true;">
edit<br />task
</a>
</td>

<td class="content">
<a
	href="delTask.do?class_ID=<%= rs.getString("class_ID") %>&idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Delete task'); return true;"
	onMouseOut="windowStatus(''); return true;">
delete<br />task
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
	href="../../jsp/tasks/AddTaskPage.jsp"
	onMouseOver="windowStatus('Add task'); return true;"
	onMouseOut="windowStatus(''); return true;">
Add task
</a>

</td>
</table>

</body>

</html>