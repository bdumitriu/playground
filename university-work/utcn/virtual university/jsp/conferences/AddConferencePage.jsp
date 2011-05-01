<%@ page language="java" import="java.util.GregorianCalendar,
				 ro.utcluj.vu.utils.Tools,
				 ro.utcluj.vu.main.IdentificationInfo" %>

<%
	GregorianCalendar cal = new GregorianCalendar();
	cal.setTimeInMillis(System.currentTimeMillis());
	int year = cal.get(GregorianCalendar.YEAR);
	int month = cal.get(GregorianCalendar.MONTH) + 1;
	int day = cal.get(GregorianCalendar.DAY_OF_MONTH);

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
	int visibility = 0;
	boolean createMainTopicsRight = false;
	boolean postAnswersRight = false;

	if (request.getParameter("name") != null)
		name = request.getParameter("name");
	if (request.getParameter("description") != null)
		description = request.getParameter("description");
	if (request.getParameter("visibility") != null)
	{
		String str = request.getParameter("visibility");
		try
		{
			visibility = Integer.parseInt(str);
		}
		catch (NumberFormatException e)
		{
			visibility = 0;
		}
		if (visibility == 2)
		{
			if ((request.getParameter("day") != null)
				&& (request.getParameter("month") != null)
				&& (request.getParameter("year") != null))
			{
				try
				{
					str = request.getParameter("day");
					day = Integer.parseInt(str);
					str = request.getParameter("month");
					month = Integer.parseInt(str);
					str = request.getParameter("year");
					year = Integer.parseInt(str);
				}
				catch (NumberFormatException e)
				{}
			}
		}
	}

	if ((request.getParameter("createMainTopicsRight") != null) &&
		(request.getParameter("createMainTopicsRight").equals("on")))
		createMainTopicsRight = true;

	if ((request.getParameter("postAnswersRight")!= null) &&
		(request.getParameter("postAnswersRight").equals("on")))
		postAnswersRight = true;

	int nrFiles = 0;
	StringBuffer params = new StringBuffer("dir=tmp/");
	params.append(session.getId());
	params.append("&returnPage=../conferences/AddConferencePage.jsp&submitAction=/addConf.do");
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
Add a new conference
</h1>

<table border="0">

<form	action="../utils/AttRedirect.jsp?<%= params %>"
	method="post"
	onReset="disable(day); disable(month); disable(year);">

<tr>
<td align="right" width="40%">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('This is the name of the conference as it will appear in the forum. You will also use this name while managing your conferences.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Conference name:
</td>
<td align="left"><input class="custom" type="text" name="name" value="<%= name %>" maxlength="50"></input></td>
</tr>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('It is here where you can briefly describe what this conference is inteded for (i.e. discussions about homeworks).'); return true;"
	onMouseOut="windowStatus(''); return true;">
Conference description:
</td>
<td align="left"><br /><textarea class="custom" name="description" rows="5" cols="40"><%= description %></textarea></td>
</tr>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('Here you have the option of making the conference appear/not appear/appear starting with a specific date in the forum.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Conference visibility:
</td>

<td align="left">
<br />
<input	type="radio"
		name="visibility"
		value="0"
<%
	if (visibility == 0)
	{
%>
		checked
<%
	}
%>
		onClick="disable(day); disable(month); disable(year);">
conference is visible in forum
</input>
<br />
<input	type="radio"
		name="visibility"
		value="1"
<%
	if (visibility == 1)
	{
%>
		checked
<%
	}
%>
		onClick="disable(day); disable(month); disable(year);">
conference is not visible in forum
</input>
<br />
<input	type="radio"
		id="date"
		name="visibility"
		value="2"
<%
	if (visibility == 2)
	{
%>
		checked
<%
	}
%>
		onClick="enable(day); enable(month); enable(year);">
conference should become visible in forum from:
</input>
<br />

<!-- day -->
<select class="custom" name="day"
<%
	if (visibility != 2)
	{
%>
disabled
<%
	}
%>
style="margin-left: 117; margin-top: 7;">
<%
	for (int i = 1; i <= 31; i++)
	{
%>
<option class="custom" value="<%= i %>"
<%
		if (i == day)
		{
%>
 selected
<%
		}
%>
>
<%
		if (i <= 9)
		{
%>
0<%= i %></option>
<%
		}
		else
		{
%>
<%= i %></option>
<%
		}
	}
%>
</select>
.

<!-- month -->
<select class="custom" name="month"
<%
	if (visibility != 2)
	{
%>
disabled
<%
	}
%>
>
<%
	for (int i = 1; i <= 12; i++)
	{
%>
<option class="custom" value="<%= i %>"
<%
		if (i == month)
		{
%>
 selected
<%
		}
%>
><%= Tools.getShortMonthName(i) %></option>
<%
	}
%>
</select>
.

<!-- year -->
<select class="custom" name="year"
<%
	if (visibility != 2)
	{
%>
disabled
<%
	}
%>
>
<%
	for (int i = year; i < year + 31; i++)
	{
%>
<option class="custom" value="<%= i %>"
<%
		if (i == year)
		{
%>
 selected
<%
		}
%>
>
<%= i %></option>
<%
	}
%>
</select>

</td>
</tr>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('Here you have the option of allowing/disallowing students to create main topics and/or post answers in the conference.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Students can:
</td>
<td align="left">

<br />
<input type="checkbox" name="createMainTopicsRight"
<%
	if (createMainTopicsRight)
	{
%>
checked
<%
	}
%>
>create main topics</input>
<br />
<input type="checkbox" name="postAnswersRight"
<%
	if (postAnswersRight)
	{
%>
checked
<%
	}
%>
>post answers inside main topics</input>
<br />
<input type="checkbox" checked disabled>read answers inside main topics</input>
<br />

</td>
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
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Create >> "></input>
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