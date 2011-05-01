<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 java.io.File,
				 ro.utcluj.vu.tasks.TaskData,
				 java.sql.ResultSet,
				 java.util.Map,
				 java.util.ArrayList,
				 ro.utcluj.vu.utils.Tools"%>

<%
	Object tmp = session.getAttribute("id");
	IdentificationInfo idInfo = null;
	String classID = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		idInfo = (IdentificationInfo) tmp;
		classID = idInfo.getClassID();
	}

	tmp = session.getAttribute("data");
	session.removeAttribute("data");
	TaskData data = null;
	if ((tmp != null) && (tmp instanceof TaskData))
	{
		data = (TaskData) tmp;
	}

	String name = "";
	String description = "";
	String difficulty = "";
	String idx = "";
	ArrayList files = null;
	int nrFiles = 0;

	if (data == null)
	{
		if (request.getParameter("name") != null)
			name = request.getParameter("name");
		if (request.getParameter("description") != null)
			description = request.getParameter("description");
		if (request.getParameter("difficulty") != null)
			difficulty = request.getParameter("difficulty");
		if (request.getParameter("idx") != null)
			idx = request.getParameter("idx");
	}
	else
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			name = rs.getString("title");
			description = rs.getString("description");
			difficulty = rs.getString("difficulty");
			idx = (String) rs.getString("idx");
			files = (ArrayList) data.getFilesMap().get(new Integer(data.getIdx()));
		}
		else
		{
			RequestDispatcher dispatcher = getServletConfig().getServletContext().
				getRequestDispatcher("/jsp/tasks/NoTask.jsp" + request.getQueryString());
			dispatcher.forward(request, response);
		}
	}

	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../tasks/DisplayTask.jsp&submitAction=/editTask2.do");
	if (data != null)
	{
		if (files != null)
		{
			nrFiles = files.size();
			params.append("&files=");
			params.append(nrFiles);
			for (int i = 0; i < nrFiles; i++)
			{
				params.append("&file" + i + "=");
				params.append(files.get(i));
			}
		}
	}
	else if (request.getParameter("files") != null)
	{
		nrFiles = Integer.parseInt(request.getParameter("files"));
		files = new ArrayList();
		params.append("&files=");
		params.append(nrFiles);
		for (int i = 0; i < nrFiles; i++)
		{
			params.append("&file" + i + "=");
			files.add(request.getParameter("file" + i));
			params.append(files.get(i));
		}
	}
	params.append("&idx=");
	params.append(idx);
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Edit task
</h1>

<table border="0">

<form	action="../../jsp/utils/AttRedirect.jsp?<%= params %>"
	method="post">

<tr>
<td align="right" width="40%">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('It is important that you give each of your tasks a distinctive name so that you can easily recognize it later.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task name:
</td>
<td align="left"><input class="custom" type="text" name="name" value="<%= Tools.replaceHTMLChars(name) %>" maxlength="50" /></td>
</tr>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('It is here where you can describe the requirements of the task. Alternatively, you could attach a file with this description.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task description:</td>
<td align="left"><br /><textarea class="custom" name="description" rows="5" cols="40"><%= description %></textarea></td>
</tr>

<tr>
<td align="right"><br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('There is no university-wide difficulty scale. You can rate your tasks as you see fit here.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task difficulty:</td>
<td align="left"><br /><input class="custom" type="text" name="difficulty" value="<%= Tools.replaceHTMLChars(difficulty) %>" maxlength="50" /></td>
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
			out.print(files.get(i));
			out.print("<input type=\"hidden\" name=\"file" + i + "\" value=\"");
			out.print(files.get(i));
			out.print("\" />");
			out.print("</b>");
			out.println(", ");
		}
		out.print("<b>");
		out.print(files.get(nrFiles - 1));
		out.print("<input type=\"hidden\" name=\"file" + (nrFiles - 1) + "\" value=\"");
		out.print(files.get(nrFiles - 1));
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
<input class="custom" type="submit" value=" << Update >> " />
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