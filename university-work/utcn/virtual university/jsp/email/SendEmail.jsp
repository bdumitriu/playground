<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.email.EmailData" %>

<%
	EmailData data = (EmailData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">


<%
	if (data.sent())
	{
%>

<h2>E-mail message has been sent.</h2>

<%
	}
	else
	{
%>
<h2>
Difficulties encountered while trying to sent the e-mail message. Please<br />
check with the system administrator that the mail server works properly.
</h2>
<%
	}
%>

<br />
<a
	href="singleEmail.do"
        onMouseOver="windowStatus('Back to previous menu'); return true;"
	onMouseOut="windowStatus(''); return true;">
Send again
</a>

</td>
</table>

</body>

</html>