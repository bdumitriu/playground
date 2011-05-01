<%@ page language="java" import="ro.utcluj.vu.conferences.MessageData, java.sql.ResultSet" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td align="center" valign="middle">

<%
	MessageData data = (MessageData) session.getAttribute("data");
%>
<%
	ResultSet rs = data.getResult();
	if ((data != null) && (data.getRowCount() == 1))
	{
%>

<h2>
Modifications to <%= data.getTitle() %> have been saved successfully.
</h2>

<%
	}
	else
	{
%>
<h2>
Error saving modifications.
</h2>
<%
	}
%>

<a
<%
	if (data.getParentID() != null)
	{
%>
	href="displayMess.do?messageid=<%= data.getParentID() %>"
<%
	}
	else if (data.getIndex() != -1)
	{
%>
	href="displayConf.do?idx=<%= data.getIndex() %>"
<%
	}
	else
	{
%>
	href="javascript:history.go(-2)"
<%
	}
%>
	onMouseOver="windowStatus('Go back to message contents'); return true;"
	onMouseOut="windowStatus(''); return true;">
Back
</a>

</td>
</table>

</body>

</html>