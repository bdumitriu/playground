<%@ page language="java" import="ro.utcluj.vu.assignments.AssignmentData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	AssignmentData data = (AssignmentData) session.getAttribute("data");
	if (data.getRowCount() == 1)
	{
%>
<h2>
Assignment has been removed successfully.
</h2>
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
	href="tchListAss.do?status=<%= request.getParameter("status") %>"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
BACK
</a>

</td>
</table>

</body>

</html>