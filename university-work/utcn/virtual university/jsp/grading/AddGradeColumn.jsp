<%@ page language="java" import="java.sql.*,
				 ro.utcluj.vu.grading.GradingData" %>

<%
	GradingData data = (GradingData) session.getAttribute("data");
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

<h2>The grade column has been created successfully.</h2>

<%
	}
	else
	{
%>
<h2>
Something went wrong while trying to create the new grade column.<br />
Please try once more and if you again encounter problems,<br />
please contact the system administrator.
</h2>
<%
	}
%>

<br />
<a
	href="displayGrades.do"
        onMouseOver="windowStatus('Back to previous menu'); return true;"
	onMouseOut="windowStatus(''); return true;">
Back
</a>

</td>
</table>

</body>

</html>
