<%@ page language="java" import="java.sql.*, ro.utcluj.vu.assignments.AssignmentData,
				 java.util.GregorianCalendar,
				 ro.utcluj.vu.utils.Tools" %>

<%
	AssignmentData data = (AssignmentData) session.getAttribute("data");

	GregorianCalendar cal = new GregorianCalendar();
	cal.setTimeInMillis(System.currentTimeMillis());
	int year = cal.get(GregorianCalendar.YEAR);
	int month = cal.get(GregorianCalendar.MONTH) + 1;
	int day = cal.get(GregorianCalendar.DAY_OF_MONTH);
	new AssignmentData();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h2>Step 3: define assignment data</h2>

<table border="0">

<form action="assTask4.do" method="post">
<input type="hidden" name="task" value="<%= data.getTaskIdx() %>" />
<%
	for (int i = 0; i < data.getStudents().size(); i++)
	{
		String str = (String) data.getStudents().get(i);
%>
<input type="hidden" name="students" value="<%= str %>" />
<%
	}
%>

<tr>
<td align="right">
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('This comment will be sent to the student as an \'extra\' to the assigned task.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Comment for student:
</td>
<td align="left" style="padding-left: 10px">
<textarea class="custom" name="studCom" rows="5" cols="40"></textarea>
</td>
</tr>

<tr>
<td align="right">
<br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('This comment is available for your personal use (you can use it as a remider of what you intended with assigning this task).'); return true;"
	onMouseOut="windowStatus(''); return true;">
Comment for yourself:
</td>
<td align="left" style="padding-left: 10px">
<br />
<textarea class="custom" name="selfCom" rows="5" cols="40"></textarea>
</td>
</tr>

<tr>
<td align="right">
<br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('The due date for the task.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Task has to be submitted before:
</td>
<td align="left" style="padding-left: 10px">
<br />

<!-- day -->
<select class="custom" name="day">
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
<select class="custom" name="month">
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
<select class="custom" name="year">
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
<br />
<img	src="../../img/help.jpg"
	onMouseOver="windowStatus('Allow/disallow student to submit his/her homework after the due date has passed.'); return true;"
	onMouseOut="windowStatus(''); return true;">
Should the student be allowed<br />
to submit his/her answer after<br />
the date specified above?
</td>
<td align="left" style="padding-left: 10px">
<br />
<input type="radio" name="permission" value="1" />yes<br />
<input type="radio" name="permission" value="2" checked />no
</td>
</tr>

<tr>
<td align="right">
<br />
<input class="custom" type="reset" value=" << Reset >> " />
</td>
<td align="left">
<br />
<input class="custom" type="submit" value=" << Finish >> " />
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