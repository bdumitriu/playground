<%@ page language="java" import="ro.utcluj.vu.conferences.ConferenceData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />

<body>

<table class="container">
<td class="container">

<%
	ConferenceData data = (ConferenceData) session.getAttribute("data");
	if (data.getRowCount() == 1)
	{
%>

<h2>
Conference has been removed successfully.
</h2>

<%
	}
	else
	{
%>
<h2>
Something went wrong while trying to delete the conference.<br />
Please try once more and if you again encounter problems,<br />
please contact the system administrator.
</h2>
<%
	}
%>

</td>
</table>

</body>

</html>