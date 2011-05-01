<%@ page language="java" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<%
	String resourceName = request.getParameter("resource_name");
%>

<body>

<table class="container">

<td class="container">

<h2>Create new <%= resourceName %> resource</h2>

<br />

<table>

<tr>
<td align="justified">
You have chosen to create the <%= resourceName %> resource.
Please choose it's type from the followings: <br /><br />

- URL: you give a url address that points to your resource.<br />
- TEXT: you edit your own data within a text field, data that should
be the content of the resource.<br />
- FILE: you choose to attach a <b>single</b> resource file, that will be
available for download.<br />
</td>
</tr>
</table>

<form
	method="post"
	action="../../jsp/classtm/ChooseResourceRedirect.jsp?resource_name=<%= resourceName %>"
	>

<table>
<tr>
<td align="right">
Choose the resource type:
</td>

<td align="left">

<input	type="radio"
		name="resource_type"
		value="url"
		>
URL resource type
</input>
<br />

<input	type="radio"
		name="resource_type"
		value="text"
		checked
		>
TEXT resource type
</input>
<br />

<input	type="radio"
		name="resource_type"
		value="file"
		>
FILE resource type
</input>
<br />

</td>

</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Next >> "></input>
</td>
</tr>
</table>

</form>

</td>
</table>

</body>

</html>