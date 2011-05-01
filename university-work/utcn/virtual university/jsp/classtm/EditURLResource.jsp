<%@ page language="java" import="java.sql.*, ro.utcluj.vu.classtm.ClassTMData,
				 ro.utcluj.vu.main.IdentificationInfo"%>

<%
	ClassTMData data = (ClassTMData) session.getAttribute("data_redirected");
	ResultSet rs = data.getResult();
	rs.beforeFirst();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	if (!rs.next())
	{
%>

<h2>No URL <%= data.getResourceName() %> information available at this time.</h2>

<%
	}
	else
	{
		if (rs != null)
		{
			String resource = data.getResourceName();
			String url = rs.getString(1);
			session.removeAttribute("data_redirected");
%>

<h2>Edit <%= resource %></h2>

<br />
The <%= resource %> resource type is URL. To edit it's value you should<br />
edit the following text field with another desired location. If you leave<br />
the field empty NULL will be considered and an exception will be raised.<br />

<table border="0" width="80%">

<br /><br />

<form
	method="POST"
	action="updateResource.do?resource_name=<%= resource %>&resource_type=url">

<tr>
<td align="right">Edit the <%= resource %> 's URL:</td>
<td align="left">
<input type="text" name="resource" value="<%= url %>" />
</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<br />
<input class="button" type="reset" value=" << Reset >> "></input>
<input class="button" type="submit" value=" << Update >> "></input>
</td>
</tr>

</form>

</table>
<%
		}
	}
%>
</table>

</body>

</html>