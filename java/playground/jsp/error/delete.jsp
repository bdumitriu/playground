<%@ include file="copyright.html" %>

<%@ page isThreadSafe="false" import="java.util.*, email.Map" errorPage="error.jsp" %>

<jsp:useBean id="mymap" scope="session" class="email.Map" />
<jsp:setProperty name="mymap" property="name" param="name" />

<!-- tags the JSP page so that we can display the right exception message later -->

<% mymap.setAction("delete"); %>

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
<td align="right">Please enter a name you would like to delete.</td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
The map file has <font color="blue"> <%= mymap.size() %></font> entries.
</td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right"><input type="submit" value="Delete"></td>
</tr>

<!-- display the name and email address, then delete them from the map file -->

<% 
	if (request.getParameter("name") != null) { 
%>

<%@ include file="deleteresponse.jsp" %> 

<%
		mymap.remove(request.getParameter("name")); 
	} 
%>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
<a href="email.jsp">Add</a>
&nbsp; | &nbsp;
<a href="lookup.jsp">Lookup</a>
</td>
</tr>

</table>

</body>

</html>