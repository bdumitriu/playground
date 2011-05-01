<%@ page language="java" %>

<%
	String resourceName = request.getParameter("resource_name");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">


<h2>Edit <%= resourceName %></h2>

<br />
The <%= resourceName %> resource type is URL. To create a new value you should<br />
edit the following text field with another desired location. If you leave<br />
the field empty NULL will be considered and an exception will be raised.<br />

<table border="0" width="80%">

<br /><br />

<form
	method="POST"
	action="createResource.do"
	>

<tr>
<td align="right">Edit the <%= resourceName %> 's URL:</td>
<td align="left">
<input type="text" name="resource" />
</td>
</tr>

<tr>
<td>
<input type="hidden" name="resource_type" value="url" />
<input type="hidden" name="resource_name" value="<%= resourceName %>" />
</tr>
</td>

<tr>
<td align="center" colspan="2">
<br />
<br />
<input class="button" type="reset" value=" << Reset >> "></input>
<input class="button" type="submit" value=" << Create >> "></input>
</td>
</tr>

</form>

</table>

</table>

</body>

</html>