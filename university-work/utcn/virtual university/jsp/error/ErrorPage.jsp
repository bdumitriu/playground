<%@ page language="java" import="ro.utcluj.vu.error.*" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
An error has occured
</h1>

<%
	Throwable error = (Throwable) session.getAttribute("error");
%>

<table border="0">

<tr>

<td align="right">
<nobr><h2>The error name:</h2></nobr>
</td>
<td align="left" style="padding-left: 10px">
<h2><%= error.getClass().getName() %></h2>
</td>

</tr>
<tr>

<td align="right">
<nobr><h2>The error message:</h2></nobr>
</td>
<td align="left" style="padding-left: 10px">
<h2><%= error.getMessage() %></h2>
</td>

</tr>

</table>

</td>
</table>

</body>

</html>