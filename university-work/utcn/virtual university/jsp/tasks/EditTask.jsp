<%@ page language="java" import="ro.utcluj.vu.tasks.TaskData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />

<body>

<table class="container">
<td class="container">

<%
	TaskData data = (TaskData) session.getAttribute("data");
	if (data.getRowCount() == 1)
	{
%>

<h2>
Task <%= data.getTitle() %> has been updated successfully.
</h2>

<%
	}
%>

</td>
</table>

</body>

</html>