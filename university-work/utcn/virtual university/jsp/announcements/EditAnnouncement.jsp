<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.announcements.AnnouncementData, ro.utcluj.vu.utils.Tools" %>

<%
	AnnouncementData data = (AnnouncementData) session.getAttribute("data");
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

<h2>Announcement has been updated successfully.</h2>

<%
	}
%>

<br />
<a
	href="listAnn2.do"
        onMouseOver="windowStatus('Back to previous menu'); return true;"
	onMouseOut="windowStatus(''); return true;">
Back
</a>


</td>
</table>

</body>

</html>