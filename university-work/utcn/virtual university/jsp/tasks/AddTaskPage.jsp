<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 java.io.File"%>

<%
	Object tmp = session.getAttribute("id");
	IdentificationInfo idInfo = null;
	String classID = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		idInfo = (IdentificationInfo) tmp;
		classID = idInfo.getClassID();
	}

	String name = "";
	String description = "";
	String difficulty = "";
	if (request.getParameter("name") != null)
		name = request.getParameter("name");
	if (request.getParameter("description") != null)
		description = request.getParameter("description");
	if (request.getParameter("difficulty") != null)
		difficulty = request.getParameter("difficulty");

	int nrFiles = 0;
	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../tasks/AddTaskPage.jsp&submitAction=/addTask.do");
	if (request.getParameter("files") != null)
	{
		nrFiles = Integer.parseInt(request.getParameter("files"));
		params.append("&files=");
		params.append(nrFiles);
		for (int i = 0; i < nrFiles; i++)
		{
			params.append("&file" + i + "=");
			params.append(request.getParameter("file" + i));
		}
	}
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Add a new task
</h1>

<table border="0">

<form	action="../utils/AttRedirect.jsp?<%= params %>"
	method="post">

<tr>
<td align="right" width="40%">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('It is important that you give each of your tasks a distinctive name so that you can easily recognize it later.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task name:
</td>
<td align="left"><input class="custom" type="text" name="name" value="<%= name %>" maxlength="50" /></td>
</tr>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('It is here where you can describe the requirements of the task. Alternatively, you could attach a file with this description.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task description:
</td>
<td align="left"><br /><textarea class="custom" name="description" rows="5" cols="40"><%= description %></textarea></td>
</tr>

<tr>
<td align="right"><br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('There is no university-wide difficulty scale. You can rate your tasks as you see fit here.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task difficulty:</td>
<td align="left"><br /><input class="custom" type="text" name="difficulty" value="<%= difficulty %>" maxlength="50" /></td>
</tr>

<tr>
<td align="right">
<br />
Attached files:
<input type="hidden" name="files" value="<%= nrFiles %>" />
</td>
<td align="left">
<br />
<%
	if (nrFiles == 0)
	{
%>
<b>none</b>
<%
	}
	else
	{
		for (int i = 0; i < nrFiles - 1; i++)
		{
			out.print("<b>");
			out.print(request.getParameter("file" + i));
			out.print("<input type=\"hidden\" name=\"file" + i + "\" value=\"");
			out.print(request.getParameter("file" + i));
			out.print("\" />");
			out.print("</b>");
			out.println(", ");
		}
		out.print("<b>");
		out.print(request.getParameter("file" + (nrFiles - 1)));
		out.print("<input type=\"hidden\" name=\"file" + (nrFiles - 1) + "\" value=\"");
		out.print(request.getParameter("file" + (nrFiles - 1)));
		out.print("\" />");
		out.println("</b>.");
		out.println();
	}
%>
</td>
</tr>

<tr>
<td colspan="2" align="center">
<br />

<input class="custom" type="submit" name="attach" value=" << Attach more files / Delete attached files >> " />

</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> " />
<input class="custom" type="submit" value=" << Create >> " />
</td>
</tr>

</form>

</table>

<br /><br /><br />
Move your mouse over the <img src="../../img/help.jpg"> image to get help in the status bar.

</td>
</table>

</body>

</html>