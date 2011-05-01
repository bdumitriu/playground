<%@ page language="java" import="java.sql.*, ro.utcluj.vu.email.EmailData,
				 java.util.ArrayList" %>


<%
	EmailData data = (EmailData) session.getAttribute("data");
	String theSender = data.getSelfEmail();
	ArrayList rcpts = data.getRcpts();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	if (rcpts == null)
	{

%>
<h1>
No recipients selected.
</h1>
<%
	}
	else
		session.setAttribute("rcpts", rcpts);
	{
%>
<h1>
Edit e-mail
</h1>

<table border="0" width="80%">

<form	action="sendMultipleEmails.do?sender=<%= theSender %>"
		method="post">

<tr>
<td align="right" width="20%">From:</td>
<td align="left"> <%= theSender %>
</td>
</tr>

<tr>
<td align="right">To:</td>
<td align="left">
<%
		for (int i = 0; i < rcpts.size()-1; i++)
		{
%>
<%= (String) rcpts.get(i) %>,
<%
		}
%>
<%= (String) rcpts.get(rcpts.size()-1) %>.
</td>
</tr>

<tr>
<td align="right">E-mail subject:</td>
<td align="left"><input type="text" class="custom" name="subject" size="70">
</input></td>
</tr>

<tr>
<td align="right"><br />E-mail message:</td>
<td align="left"><br /><textarea class="custom" name="message" rows="13" cols="70">
</textarea></td>
</tr>



<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Send >> "></input>
</td>
</tr>

</form>

</table>

<%
	}
%>

</td>
</table>

</body>

</html>