<%@ page language="java" import="ro.utcluj.vu.assignments.AssignmentData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	AssignmentData data = (AssignmentData) session.getAttribute("data");
	if ((data != null) && (data.getRowCount() > 0))
	{
%>

<h2>
Answer submitted successfully.
</h2>

<%
	}
	else
	{
%>
<h2>
Error submitting answer.
</h2>
<%
	}
%>

<a
	href="studListAss.do"
	onMouseOver="windowStatus('Back to assignments list'); return true;"
	onMouseOut="windowStatus(''); return true;">
BACK
</a>

</td>
</table>

</body>

</html>