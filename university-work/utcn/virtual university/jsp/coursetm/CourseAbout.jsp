<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.coursetm.CourseTMData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	CourseTMData data = (CourseTMData) session.getAttribute("data");
%>

<h2>
	Following the <%= data.getCourseName() %> course.
	<br />
	<br />
	@author <%= data.getAuthorName() %>.
	<br />
	@author e-mail:
	<a href="mailto:<%= data.getAuthorEmail() %>"><%= data.getAuthorEmail() %></a>.
</h2>

</td>

</table>

</body>

</html>