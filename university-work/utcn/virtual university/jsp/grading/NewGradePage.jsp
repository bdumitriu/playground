<%@ page language="java" %>

<%
	String studentID = request.getParameter("student_ID");
	String columnID = request.getParameter("column_ID");
	String studentName = request.getParameter("student_name");
	String columnName = request.getParameter("column_name");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h2>Add grade<h2>

<table>
<tr>
<td align="justified">
Add a new grade for <%= studentName %> on the <%= columnName %> field.
</td>
</tr>
<tr><br /></tr>
</table>

<form method="POST" action="addGrade.do">

<table>

<tr>
<td align="right">Enter the grade value here:</td>
<td align="left">
<input type="text" name="grade" />
</td>
</tr>

<tr>
<td>
<input type="hidden" name="student_ID" value="<%= studentID %>" />
</td>
<td>
<input type="hidden" name="column_ID" value="<%= columnID %>" />
</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<br />
<input class="button" type="reset" value=" << Reset >> "></input>
<input class="button" type="submit" value=" << Edit >> "></input>
</td>
</tr>

</table>

</form>

</td>
</table>

</body>

</html>