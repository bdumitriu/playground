<%@ page language="java" import="ro.utcluj.vu.grading.GradingData,
				 java.sql.ResultSet"%>

<%
	GradingData data = (GradingData) session.getAttribute("data");
	ResultSet rs = data.getResult();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	if ((!rs.next()) || (rs == null))
	{
%>

<h2>No grade columns available at this time.</h2>

<%
	}
	else
	{
		String columnName;
		String columnID;
		String classID;
		java.sql.Date date;
%>
<h2>Delete grade column</h2>

<form method="post" action="">
<table>
<%
		rs.beforeFirst();
		while (rs.next())
		{
			columnName = rs.getString("column_name");
			columnID = rs.getString("column_ID");
			classID = rs.getString("class_ID");
			date = rs.getDate("creation_date");
%>
<tr>
<td align="right">
Delete grade column:
</td>
<td align="left">
<a	href="deleteGradeColumn.do?class_ID=<%= classID %>&column_ID=<%= columnID %>"
	onMouseOver="windowStatus('Delete grade column'); return true;"
	onMouseOut="windowStatus(''); return true">
<%= columnName %>,&nbsp; Created on &nbsp; <%= date.toString() %>
</a>
</td>
</tr>

<%
		}
	}
%>

</table>
</form>

</td>

</table>

</body>

</html>