<%@ page language="java" import="ro.utcluj.vu.conferences.MessageData, java.util.*, java.sql.ResultSet, ro.utcluj.vu.utils.Tools" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	MessageData data = (MessageData) session.getAttribute("data");
	ResultSet rs = data.getResult();
	if (rs != null)
	{
		if (rs.next())
		{
%>

<h1>
Edit message
</h1>

<table border="0">

<form	action="editMess2.do?messageid=<%= request.getParameter("messageid") %>"
		method="post">
<tr>

<td align="right">Message title:</td>
<td align="left">
<input
	class="custom"
	type="text"
	name="title"
	value="<%= rs.getString("title") %>"
	maxlength="50">
</input>
</td>
</tr>

<tr>
<td align="right">Message text:</td>
<td align="left">
<br />
<textarea class="custom" name="contents" rows="5" cols="40">
<%= rs.getString("message") %>
</textarea>
</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Update >> "></input>
</td>
</tr>

</form>

</table>

<%
		}
	}
%>

</td>
</table>

</body>

</html>