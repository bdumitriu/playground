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

	String answer = "";
	int assIdx =  -1;
	if (request.getParameter("answer") != null)
		answer = request.getParameter("answer");
	if (request.getParameter("idx") != null)
	{
		try
		{
			assIdx = Integer.parseInt(request.getParameter("idx"));
		}
		catch (NumberFormatException e)
		{
			assIdx = -1;
		}
	}

	int nrFiles = 0;
	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../assignments/SubmitAnswerPage.jsp&submitAction=/submitAnswer.do&idx=");
	params.append(assIdx);
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
Submit your answer
</h1>

<table border="0">

<form	action="../utils/AttRedirect.jsp?<%= params %>"
	method="post">

<tr>
<td align="right" width="40%">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('You can give your answer here if it\'s up to 5000 characters long or send it as an attachement if it\'s longer.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Your answer:
</td>
<td align="left"><textarea class="custom" name="answer" rows="5" cols="40"><%= answer %></textarea></td>
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
<input class="custom" type="submit" value=" << Submit >> " />
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