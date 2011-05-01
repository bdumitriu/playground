<%@ page language="java" import="java.util.GregorianCalendar,
				 ro.utcluj.vu.utils.Tools" %>

<%
	GregorianCalendar cal = new GregorianCalendar();
	cal.setTimeInMillis(System.currentTimeMillis());
	int year = cal.get(GregorianCalendar.YEAR);
	int month = cal.get(GregorianCalendar.MONTH) + 1;
	int day = cal.get(GregorianCalendar.DAY_OF_MONTH);
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Add announcement
</h1>

<table border="0">

<form	action="addAnn.do"
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
<td align="left"><br /><textarea class="custom" name="announcement" rows="5" cols="40"></textarea></td>
</tr>

<tr>
<td align="right">Announcement visibility:</td>
<td align="left">

<br />
<input	type="radio"
		name="visibility"
		value="0"
		checked
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
	disabled
	style="margin-left: 10"
	onClick="changeState(from, day_s); changeState(from, month_s); changeState(from, year_s);"/>
</td>

<td align="right">
from:
</td>

<td align="left">

<!-- day -->
<select class="custom" name="day_s" disabled>
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
<select class="custom" name="month_s" disabled>
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
<select class="custom" name="year_s" disabled>
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

<td align="right" style="margin-left: 10">
<input
	type="checkbox"
	name="until"
	disabled
	style="margin-left: 10"
	onClick="changeState(until, day_f); changeState(until, month_f); changeState(until, year_f);"/>
</td>

<td align="right">
until:
</td>

<td align="left">
<!-- day -->
<select class="custom" name="day_f" disabled>
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
<select class="custom" name="month_f" disabled>
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
<select class="custom" name="year_f" disabled>
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

</table>

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

</td>
</table>

</body>

</html>