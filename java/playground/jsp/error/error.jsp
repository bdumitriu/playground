<%@ include file="copyright.html" %>

<%@ page isErrorPage="true" import="java.util.*, email.Map" %>

<jsp:useBean id="mymap" scope="session" class="email.Map />

<html>

<head>
<title>Email Finder</title>
</head>

<body bgcolor="#ffffff" background="back.gif" link="#000099">

<table border="0" cellspacing="0" cellpadding="5">

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right" valign="bottom"><h1> Email Finder</h1></td>
</tr>

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right"> <b>Oops! an exception occurred.</b></td>
</tr>

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right">The name of the exception is <%= exception.toString() %>.</td>
</tr>

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right"> &nbsp; </td>
</tr>

<%
	if (mymap.getAction() == "delete") { 
%>

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right">
<b><i>This means that... </i></b>
<p>the entry you were trying to <font color="blue">delete</font> is not in the map file<br>
<b><i>or</i></b><br>
you did not enter a name to delete. <p> Want to try <a href="delete.jsp">again</a>?
</td>
</tr>

<%
	} else if (mymap.getAction() == "lookup" ) { 
%> 

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right">
<b><i>This means that... </i></b>
<p>the entry you were trying to <font color="blue">look up</font> is not in the map file<br>
<b><i>or</i></b><br>
you did not enter a name to look up. <p> Want to try <a href="lookup.jsp">again</a>?
</td>
</tr> 

<%
	} else if (mymap.getAction() == "add") { 
%>

<tr>
<td width="120" align="right"> &nbsp; </td>
<td align="right">
<b><i>This means that... </i></b>
<p>you were trying to <font color="blue">add</font> an entry with a name of null.<br>
The map file doesn't allow this. <p> Want to try <a href="email.jsp">again</a>?
</td>
</tr> 

<%
	}
%>

</table>

</body>

</html>