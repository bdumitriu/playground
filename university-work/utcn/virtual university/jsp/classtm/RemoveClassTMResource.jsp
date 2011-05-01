<%@ page language="java" import="java.sql.*,
				 ro.utcluj.vu.classtm.ClassTMData" %>

<%
	ClassTMData data = (ClassTMData) session.getAttribute("data");
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

<h2>The <%= data.getResourceName() %> class resource has been removed successfully.</h2>

<%
	}
	else
	{
%>
<h2>
Something went wrong while trying to delete the <%= data.getResourceName() %> class resource.<br />
Please try once more and if you again encounter problems,<br />
please contact the system administrator.
</h2>
<%
	}
%>

<br />
<a
	href="getClassTM.do"
        onMouseOver="windowStatus('Back to previous menu'); return true;"
	onMouseOut="windowStatus(''); return true;">
Back
</a>

</td>
</table>

</body>

</html>