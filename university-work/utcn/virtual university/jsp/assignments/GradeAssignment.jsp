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
	if (data.getRowCount() == 1)
	{
%>
<h2>The assignment has been graded.</h2>
<%
	}
	else
	{
%>
<h2>
An error has occured while performing operation. Please check<br />
the current configuration of the assignments and, if anything<br />
seems to be wrong, please contact the system administrator.<br />
We appologize for the inconvenience.
</h2>
<%
	}
%>
<a
	href="tchListAss.do?status=<%= data.getStatus() %>"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
BACK
</a>

</td>
</table>

</body>

</html>