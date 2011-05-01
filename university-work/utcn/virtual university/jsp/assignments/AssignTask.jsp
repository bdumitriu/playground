<%@ page language="java" import="java.sql.*, ro.utcluj.vu.assignments.AssignmentData,
				 java.util.ArrayList" %>

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
	ArrayList students = data.getStudents();

	// normally this can't happen (or we wouldn't be here) but who knows...
	if (students == null)
	{
%>
<h2>No assignement has been made.</h2>
<%
	}
	else if (students.size() == data.getRowCount())
	{
%>
<h2>The task has been successfully assigned.</h2>
<%
	}
	else
	{
%>
<h2>
Out of a total of <%= students.size() %> requested assignments,<br />
only <%= data.getRowCount() %> have been made successfully. Please<br />
check which students haven't been assigned the task and try again.<br />
We appologize for the inconvenience.
</h2>
<%
	}
%>

<a
	href="assTask1.do"
	onMouseOver="windowStatus('Assign another task'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>Assign another task</nobr>
</a>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a
	href="tchListAss.do?status=1"
	onMouseOver="windowStatus('Go to unsubmitted assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>View unsubmitted assignments</nobr>
</a>

</td>
</table>

</body>

</html>