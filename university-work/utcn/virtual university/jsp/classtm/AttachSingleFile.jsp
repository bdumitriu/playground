<%@ page language="java" import="ro.utcluj.vu.classtm.AttachSingleFileData" %>

<%
	Object tmp = session.getAttribute("data");
	AttachSingleFileData data;
	String attachedFile;

	// a self call has taken place
	if ((tmp != null) && (tmp instanceof AttachSingleFileData))
	{
		data = (AttachSingleFileData) tmp;
		attachedFile = data.getFileName();
	}
	else // it's the first call
	{
		data = new AttachSingleFileData();
		data.setDirectory(request.getParameter("dir"));
		data.setReturnPage(request.getParameter("returnPage"));
		data.setExtraParams(request);
		attachedFile = null;
	}

	StringBuffer params = new StringBuffer("?dir=");

	StringBuffer file = new StringBuffer();

	String otherParams = data.getExtraParamsAsQueryString();

	params.append(data.getDirectory());
	params.append("&returnPage=");
	params.append(data.getReturnPage());

	if (attachedFile != null)
	{
		file.append("&file=");
		file.append(attachedFile);
		params.append(file);
	}
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
the <b>Attach</b> button to upload the file to our server. Note that uploading a new file will
delete the old attached one. This constraint is necessary due to the logical reasons of this service.
Click the "Done" button when sattisfied with the file choice. Please let us bring to your attention that
the files you upload using this screen will be moved to their final location only when you
click the <b>Create</b> button in the main screen (the one that brought you here)
and not when you click the <b>Done</b> button here.
</td>
</tr>
</table>

<table>
<form	action="attSingleFile.do<%= params %>&<%= otherParams %>&fileSubmitted="
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
<i>Already attached file:</i>
<br /><br />
<%
	if (attachedFile == null)
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
			out.println("<tr>");
			out.println("<td align=\"right\"></td>");
			out.println("<td align=\"left\"><b>" + ((String) attachedFile) + "</b></td>");
			out.print("<td style=\"padding-left: 10\">");
%>
<a href="attSingleFile.do<%= params %>&<%= otherParams %>&fileDeleted=<%= attachedFile %>">
<%
			out.println("remove this file</a></td>");
			out.println("</tr>");
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
<form action="<%= request.getParameter("returnPage") %>?<%= file %>&<%= otherParams %>" method="post">
<input class="custom" type="submit" name="done" value=" << Done >> ">
</form>
</td>
</tr>

</table>

</td>
</table>

</body>
</html>
