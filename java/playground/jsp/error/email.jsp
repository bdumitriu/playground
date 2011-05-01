<%@ include file="copyright.html" %>

<%@ page isThreadSafe="false" import="java.util.*, email.Map" errorPage="error.jsp" %>

<jsp:useBean id="mymap" scope="session" class="email.Map" />
<jsp:setProperty name="mymap" property="name" param="name" />
<jsp:setProperty name="mymap" property="email" param="email" />

<% mymap.setAction("add"); %>

<html>

<head>
<title>Email Finder</title>
</head>

<body bgcolor="#ffffff" background="back.gif" link="#000099">

<!-- the form table -->

<form method="get">
<table border="0" cellspacing="0" cellpadding="5">

<tr>
<td width="97"> &nbsp; </td>
<td align="right"> <h1>Email Finder</h1> </td>
</tr>

<tr>
<td width="97" align="right"><b>Name</b></td>
<td align="left"><input type="text" name="name" size="40"></td>
</tr>

<tr>
<td width="97" align="right"><b>Email Address</b></td>
<td align="left"><input type="text" name="email" size="40"></td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
Please enter a name and an email address.
</td>
</tr>

<tr>
<td width="97"> &nbsp; </td>
<td align="right">
<input type="submit" value="Add">
</td>
</tr>

<!-- here we call the put method to add the 
          name and email address to the map file -->

<%
	String rname = request.getParameter("name");
	String remail = request.getParameter("email");
	if (rname != null) {
		mymap.put(rname, remail);
	}
%>

<tr>
<td width="120"> &nbsp; </td>
<td align="right">
The map file has <font color="blue"><%= mymap.size() %>
</font> entries.
</font>
</td>
</tr>

<tr>
<td width="120"> &nbsp; </td>
<td align="right">
<a href="lookup.jsp">Lookup</a>&nbsp; | &nbsp;
	<a href="delete.jsp">Delete</a>
</td>
</tr>

</table>
</form>

</body>
</html>
