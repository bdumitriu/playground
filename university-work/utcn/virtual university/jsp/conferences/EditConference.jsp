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
Conference <%= data.getName() %> has been updated successfully.
</h2>

<%
	}
%>

</td>
</table>

</body>

</html>