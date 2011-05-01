<%@ page language="java" import="ro.utcluj.vu.utils.AttachFilesData,
				 java.util.ArrayList" %>

<%
	Object tmp = session.getAttribute("data");
	AttachFilesData data = null;
	ArrayList files = null;
	if ((tmp != null) && (tmp instanceof AttachFilesData))
	{
		data = (AttachFilesData) tmp;
		files = data.getFileNames();
	}
	else
	{
		data = new AttachFilesData();
		data.setDirectory(request.getParameter("dir"));
		data.setReturnPage(request.getParameter("returnPage"));
		data.setExtraParams(request);
		files = new ArrayList();
	}

	StringBuffer params = new StringBuffer("?dir=");
	StringBuffer fileList = new StringBuffer("&files=");
	String otherParams = data.getExtraParamsAsQueryString();

	params.append(data.getDirectory());
	params.append("&returnPage=");
	params.append(data.getReturnPage());

	fileList.append(files.size());

	for (int i = 0; i < files.size(); i++)
		fileList.append("&file" + i + "=" + ((String) files.get(i)));

	params.append(fileList);


%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Attach files
</h1>

<table width="65%">
<tr>
<td align="justified">
<br />
Click the <b>Browse</b> button to select the file you want to upload (if you don't have such a
button, it means your browser doesn't support file upload). Once you have selected a file, click
the <b>Attach</b> button to upload the file to our server. Repeat this until you upload all the
desired files and then click the "Done" button. Please let us bring to your attention that
the files you upload using this screen will be moved to their final location only when you
click the <b>Create</b> button in the main screen (the one that brought you here)
and not when you click the <b>Done</b> button here.
</td>
</tr>
</table>

<table border="0">

<form	action="attFiles.do<%= params %>&<%= otherParams %>&fileSubmitted="
	enctype="multipart/form-data"
	method="post">

<tr>
<td align="right">
<br />
File name:
</td>
<td align="left">
<br />
<input class="custom" type="file" name="filename">
</td>
</tr>

<tr>
<td colspan="2" align="left">
<br />
<input class="button" type="submit" name="attach" value=" << Attach >> ">
</td>
</tr>

</form>

<tr>
<td colspan="2" align="left">
<br />
<i>Already attached files:</i>
<br /><br />
<%
	if (files.size() == 0)
	{
%>
<b>none</b>
<%
	}
	else
	{
%>
<table border="0">
<%
		for (int i = 0; i < files.size(); i++)
		{
			out.println("<tr>");
			out.println("<td align=\"right\">" + (i + 1) + ".</td>");
			out.println("<td align=\"left\"><b>" + ((String) files.get(i)) + "</b></td>");
			out.print("<td style=\"padding-left: 10\">");
%>
<a href="attFiles.do<%= params %>&<%= otherParams %>&fileDeleted=<%= i %>">
<%
			out.println("remove this file</a></td>");
			out.println("</tr>");
		}
%>
</table>
<%
	}
%>

</td>
</tr>

<tr>
<td colspan="2" align="left">
<br />
<form action="<%= request.getParameter("returnPage") %>?<%= fileList %>&<%= otherParams %>" method="post">
<input class="custom" type="submit" name="done" value=" << Done >> ">
</form>
</td>
</tr>

</table>

</td>
</table>

</body>
</html>
