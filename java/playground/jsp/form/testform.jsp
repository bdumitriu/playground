<%@ page import="hello.NameHandler" %>

<jsp:useBean id="mybean" scope="page" class="hello.NameHandler" />

<jsp:setProperty name="mybean" property="*" />

<html>
<head>
<title>Hello, user!</title>
</head>

<body bgcolor="#ffffff" background="back.gif">

<%@ include file="table.html" %>

<table border=0 width=700>
<tr>
<td width=150> &nbsp; </td>
<td width=550><h1>I'm The Frog! Who are you?</h1></td>
<tr>
<td width=150> &nbsp; </td>
<td width=550>
<form method=get>
<input type=text name=username size=25>
<br>
<input type=submit value=Submit>
<input type=reset value=Reset>
</td>
</tr>
</form>
</table>

<%
	if (request.getParameter("username") != null) {
%>

<%@ include file="response.jsp" %>

<%
	}
%>

</body>
</html>