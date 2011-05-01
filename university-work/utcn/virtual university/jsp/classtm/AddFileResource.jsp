<%@ page language="java" %>


<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	String resourceName = request.getParameter("resource_name");

	// get the parameters carried arround
	String newFilename = request.getParameter("file");

	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../../jsp/classtm/AddFileResource.jsp&submitAction=/createResource.do");

	if (!(newFilename == null))
	{
		params.append("&file=");
		params.append(newFilename);
	}
%>

<h2>Edit <%= resourceName %></h2>

<br />
The <%= resourceName %> resource type is FILE. Be advised that because of<br />
the simbolic limitations that one file and one only can be bound to any<br />
of the resources, when the Done button will be engaged the old attached<br/>
file will be lost, and insted the new file will be found.


<form
	method="POST"
	action="../../jsp/classtm/AttachSingleFileRedirect.jsp?<%= params %>">

<table border="0" width="80%">

<br /><br />

<%
	if (newFilename == null)
	{
%>
<tr>
<td align="center">Choose to attach a new file.</td>
<td align="left"><br /></td>
</tr>

<%
	}
	else
	{
%>

<tr>
<td align="right">The file you want to attach:</td>
<td align="left"><b><%= newFilename %></b></td>
</tr>

<tr>
<td>
<input type="hidden" name="resource" value="<%= newFilename %>" />
</td>
</tr>

<%
	}
%>

<tr>
<td colspan="2" align="center">
<br />

<input class="custom" type="submit" name="attach" value=" << Attach new file / Delete attached file >> " />

</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="submit" value=" << Create >> " />
</td>
</tr>

<tr>
<td>

<input type="hidden" name="resource_name" value="<%= resourceName %>" />
<input type="hidden" name="resource_type" value="fil" />
</td>
</tr>
</form>

<tr><td><br /></td></tr>


</table>

</td>

</table>

</body>

</html>
