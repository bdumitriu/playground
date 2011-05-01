<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.conferences.ConferenceData" %>

<%
	ConferenceData data = (ConferenceData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/conferences/reorder.js"></script>

<body>

<table class="container">
<td class="container">

<h2>
New order has been successfully saved.
</h2>

</td>
</table>

</body>

</html>