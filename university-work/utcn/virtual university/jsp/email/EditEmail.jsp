<%@ page language="java" import="java.sql.*, ro.utcluj.vu.email.EmailData" %>


<%
	String sender = request.getParameter("self");
	String rcpt = request.getParameter("rcpt");
	String userID = request.getParameter("user_ID");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Edit e-mail
</h1>

<table border="0">

<form	action="sendEmail.do?sender=<%= sender %>&rcpt=<%= rcpt %>&user_ID=<%= userID %>"
		method="post">

<tr>
<td align="right">From:</td>
<td align="left"><%= sender %></td>
</tr>

<tr>
<td align="right">To:</td>
<td align="left"><%= rcpt %></td>
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

</td>
</table>

</body>

</html>