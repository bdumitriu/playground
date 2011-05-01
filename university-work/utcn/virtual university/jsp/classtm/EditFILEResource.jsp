<%@ page language="java" import="java.sql.*, ro.utcluj.vu.classtm.ClassTMData,
				 ro.utcluj.vu.main.IdentificationInfo"%>


<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	// Verify the identification tokens.
	Object tmp = session.getAttribute("id");
	IdentificationInfo idInfo = null;
	String classID = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		idInfo = (IdentificationInfo) tmp;
		classID = idInfo.getClassID();
	}

	// parameters needed.
	String oldFilename = null;
	String resource = null;
	String newFilename = null;

	// get the redirected data - this happens the first time this page is called.
	ClassTMData data;
	if ((data = (ClassTMData) session.getAttribute("data_redirected")) != null)
	{
		ResultSet rs = data.getResult();
		rs.beforeFirst();
		if (!rs.next())
		{
%>

<h2>No FILE <%= data.getResourceName() %> information available at this time.</h2>

<%
		}
		else
		{
			if (rs != null)
			{
				resource = data.getResourceName();
				oldFilename = rs.getString(1);
				session.removeAttribute("data_redirected");
				newFilename = null;
			}
		}
	}
	else // get the parameters carried arround
	{
		oldFilename = request.getParameter("resource_description");
		resource = request.getParameter("resource_name");
		newFilename = request.getParameter("file");
	}

	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../../jsp/classtm/EditFILEResource.jsp&submitAction=/updateResource.do");

	if (!(newFilename == null))
	{
		params.append("&file=");
		params.append(newFilename);
	}
%>

<h2>Edit <%= resource %></h2>

<br />
The <%= resource %> resource type is FILE. Be advised that because of<br />
the simbolic limitations that one file and one only can be bound to any<br />
of the resources, when the Done button will be engaged the old attached<br/>
file will be lost, and insted the new file will be found.

<table border="0" width="80%">

<br /><br />

<form
	method="POST"
	action="../../jsp/classtm/AttachSingleFileRedirect.jsp?<%= params %>">

<tr>
<td align="right">Choose a file for <%= resource %> resource </td>
<td align="left"></td>
</tr>


<%
	if (newFilename == null)
	{
%>
<tr>
<td align="right">Choose to attach a new file.</td>
<td align="left"><b></b></td>
</tr>

<tr>
<td>
<input type="hidden" name="resource" value="<%= oldFilename %>" />
</td>
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
<input class="custom" type="submit" value=" << Update >> " />
</td>
</tr>

<tr>
<td>
<input type="hidden" name="resource_description" value="<%= oldFilename %>" />
<input type="hidden" name="resource_name" value="<%= resource %>" />
<input type="hidden" name="resource_type" value="fil" />
</td>
</tr>
</form>

<tr><td><br /></td></tr>

<tr>
<td align="right">Curently attached file:</td>
<td align="left"><b><%= oldFilename %></b></td>
</tr>


</table>

</table>

</body>

</html>
