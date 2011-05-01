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
The <%= resourceName %> resource type is TEXT. To create a new value you <br />
should edit the following text area with another desired text. If you leave<br />
the area empty NULL will be considered and an exception will be raised.<br />

<table border="0" width="80%">

<br /><br />

<form
	method="POST"
	action="createResource.do"
	>

<tr>
<td align="right">Edit the <%= resourceName %> 's value:</td>
<td align="left">

<textarea
	class="custom"
	name="resource"
	rows="13"
	cols="70">

</textarea>

</td>
</tr>

<tr><td>
<input type="hidden" name="resource_name" value="<%= resourceName %>" />
<input type="hidden" name="resource_type" value="txt" />
</td></tr>

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