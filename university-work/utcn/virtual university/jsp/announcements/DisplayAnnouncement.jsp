<%@ page language="java" import="java.sql.*, ro.utcluj.vu.announcements.AnnouncementData, ro.utcluj.vu.utils.*,
				 java.util.GregorianCalendar" %>

<%
	AnnouncementData data = (AnnouncementData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Edit announcement
</h1>

<table border="0">

<form	action="editAnn2.do?announcement_ID=<%= data.getAnnouncementID() %>"
		method="post"
		onReset="
			disable(from);
			disable(until);
			disable(day_f);
			disable(month_f);
			disable(year_f);
			disable(day_s);
			disable(month_s);
			disable(year_s);
		">

<tr>
<td align="right">Announcement description:</td>
<td align="left"><br /><textarea class="custom" name="announcement" rows="5" cols="40">
<%= data.getAnnouncement() %>
</textarea></td>
</tr>

<tr>
<td align="right">Announcement visibility:</td>
<td align="left">

<br />
<input	type="radio"
		name="visibility"
		value="0"
		<%
			if ((!data.isStartDateEnabled()) && (!data.isEndDateEnabled()))
			{
		%>
		checked
		<%
			}
		%>
		onClick="
			disable(from);
			disable(until);
			disable(day_f);
			disable(month_f);
			disable(year_f);
			disable(day_s);
			disable(month_s);
			disable(year_s);
		">
announcement is visible
</input>

<br />
<input	type="radio"
		id="date_s"
		name="visibility"
		value="1"
		<%
			if ((data.isStartDateEnabled()) || (data.isEndDateEnabled()))
			{
		%>
				checked
		<%
			}
		%>
		onClick="
			enable(from);
			enable(until);
			changeState(until, day_f);
			changeState(until, month_f);
			changeState(until, year_f);
			changeState(from, day_s);
			changeState(from, month_s);
			changeState(from, year_s);
		">
announcement should be visible
</input>
<br />

<table border="0">

<tr>

<td align="right">
<input
	type="checkbox"
	name="from"
	<%
		if (data.isStartDateEnabled())
		{
	%>
			checked
	<%
		}
		else
		{
	%>
			disabled
	<%
		}
	%>
	style="margin-left: 10"
	onClick="changeState(from, day_s); changeState(from, month_s); changeState(from, year_s);"/>
</td>

<td align="right">
from:
</td>

<td align="left">
<select class="custom" name="day_s"
<%
	if (!(data.isStartDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>
<%
	int day, month, year;
	if (data.isStartDateEnabled())
	{
		day = data.getStartDay();
		month = data.getStartMonth();
		year = data.getStartYear();
	}
	else
	{
		GregorianCalendar cal = new GregorianCalendar();
		day = cal.get(GregorianCalendar.DAY_OF_MONTH);
		month = cal.get(GregorianCalendar.MONTH)+1;
		year = cal.get(GregorianCalendar.YEAR);
	}
	for (byte i = 1; i <= 31; i++)
	{
		if (day == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= Tools.format2Digit(i) %></option>
<%
		}
		else
		{
%>
<option class="custom" value="<%= i %>"><%= Tools.format2Digit(i) %></option>
<%
		}
	}
%>

</select>
.
<select class="custom" name="month_s"
<%
	if (!(data.isStartDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>
<%
	for (byte i = 1; i <= 12; i++)
	{
		if (month == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= Tools.getShortMonthName(i) %></option>
<%
		}
		else
		{
%>
<option class="custom" value="<%= i %>"><%= Tools.getShortMonthName(i) %></option>
<%
		}
	}
%>
</select>
.
<select class="custom" name="year_s"
<%
	if (!(data.isStartDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>
<%
	GregorianCalendar cal = new GregorianCalendar();
	int year_start = cal.get(GregorianCalendar.YEAR);
	for (int i = year_start; i < year_start+31; i++)
	{
		if (year == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= i %></option>
<%
		}
		else
		{
%>
<option class="custom" value="<%= i %>"><%= i %></option>
<%
		}
	}
%>
</select>
</td>

</tr>

<tr>

<td align="right" style="margin-left: 10">
<input
	type="checkbox"
	name="until"
	<%
		if (data.isEndDateEnabled())
		{
	%>
			checked
	<%
		}
		else
		{
	%>
			disabled
	<%
		}
	%>
	style="margin-left: 10"
	onClick="changeState(until, day_f); changeState(until, month_f); changeState(until, year_f);"/>
</td>

<td align="right">
until:
</td>

<td align="left">
<select class="custom" name="day_f"
<%
	if (!(data.isEndDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>

<%
	if (data.isEndDateEnabled())
	{
		day = data.getEndDay();
		month = data.getEndMonth();
		year = data.getEndYear();
	}
	else
	{
		cal = new GregorianCalendar();
		day = cal.get(GregorianCalendar.DAY_OF_MONTH);
		month = cal.get(GregorianCalendar.MONTH)+1;
		year = cal.get(GregorianCalendar.YEAR);
	}
	for (byte i = 1; i <= 31; i++)
	{
		if (day == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= Tools.format2Digit(i) %></option>
<%
		}
		else
		{
%>
<option class="custom" value="<%= i %>"><%= Tools.format2Digit(i) %></option>
<%
		}
	}
%>
</select>
.
<select class="custom" name="month_f"
<%
	if (!(data.isEndDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>
<%
	for (byte i = 1; i <= 12; i++)
	{
		if (month == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= Tools.getShortMonthName(i) %></option>
<%
		}
		else
		{
%>
<option class="custom" value="<%= i %>"><%= Tools.getShortMonthName(i) %></option>
<%
		}
	}
%>
</select>
.
<select class="custom" name="year_f"
<%
	if (!(data.isEndDateEnabled()))
	{
%>
		disabled
<%
	}
%>
>
<%
	for (int i = year_start; i < year_start+31; i++)
	{
		if (year == i)
		{
%>
<option class="custom" selected value="<%= i %>"><%= i %></option>
<%
		}
		else
		{
%>
			<option class="custom" value="<%= i %>"><%= i %></option>
<%
		}
	}
%>
</select>
</td>

</tr>

</table>

</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Update >> "></input>
</td>
</tr>

</form>

</table>

</td>
</table>

</body>

</html>