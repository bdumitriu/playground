<%@ include file="copyright.html" %>

<%@ page isThreadSafe="false" import="java.util.*, email.Map" errorPage="error.jsp" %>

<jsp:useBean id="mymap" scope="session" class="email.Map" />
<jsp:setProperty name="mymap" property="name" param="name" />

<% mymap.setAction("lookup"); %>

<html>

<head>
<title>Email Finder</title>
</head>

<body bgcolor="#ffffff" background="back.gif" link="#000099">
<form method="get">
<table border="0" cellspacing="0" cellpadding="5">

<tr>
<td width="97"> &nbsp; </td>
<td align="right"><h1>Email Finder</h1></td>
</tr>

<tr>
<td width="97" align="right"><b>Name</b></td>
<td align="left"><input type="text" name="name" size="40"></td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
Please enter a name for which you'd <br> like an email address.
</td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
The map file has <font color="blue"> <%= mymap.size() %></font> entries.
</td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right"><input type="submit" value="Lookup"></td>
</tr>

<%
	if (request.getParameter("name") != null) {
%>

<%@ include file="lookupresponse.jsp" %>

<%
	}
%>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
<a href="email.jsp">Add</a>
 &nbsp; | &nbsp; 
<a href="delete.jsp">Delete</a>
</td>
</tr>

</table>

</body>

</html> 