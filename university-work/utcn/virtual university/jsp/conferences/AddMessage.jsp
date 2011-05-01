<%@ page language="java" import="ro.utcluj.vu.conferences.MessageData" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	MessageData data = (MessageData) session.getAttribute("data");
	if ((data != null) && (data.getRowCount() == 1))
	{
%>

<h2>
Message <%= data.getTitle() %> has been saved.
</h2>

<%
	}
	else
	{
%>
<h2>
Error saving message.
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