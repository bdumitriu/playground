<%@ page language="java" import="ro.utcluj.vu.conferences.MessageData,
				 java.sql.ResultSet,
				 java.util.GregorianCalendar,
				 ro.utcluj.vu.utils.Tools,
				 ro.utcluj.vu.main.IdentificationInfo" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	MessageData data = (MessageData) session.getAttribute("data");
	ResultSet rs = data.getResult();
	IdentificationInfo id = (IdentificationInfo) session.getAttribute("id");
	if (rs != null)
	{
		if (!rs.next())
		{
			if (data.isMainTopic())
			{
%>
<h2>There are no main topics in this conference yet.</h2>
<%
			}
			else
			{
%>
<h2>There are no replies to this message.</h2>
<%
			}
		}
		else
		{
%>

<%
			if (data.isMainTopic())
			{
%>
<h1>List of main topics:</h1>
<%
			}
			else
			{
%>
<h1>List of replies:</h1>
<%
			}
%>

<table border="0">

<tr>

<td>
</td>
<td class="header">
Title
</td>
<td class="header">
Author
</td>
<td class="header">
Created on
</td>
<td class="header">
Action to take
</td>

</tr>

<%
			do
			{
%>

<tr>

<td align="right">
<%
				if (rs.getBoolean("newFlag"))
				{
%>
<img src="../../img/r_bullet2.jpg" />
<%
				}
				else
				{
%>
<img src="../../img/g_bullet2.jpg" />
<%
				}
%>
</td>

<td class="content">
<nobr>
<a	class="normal"
	href="displayMess.do?messageid=<%= rs.getString("message_id") %>"
	onMouseOver="windowStatus('Click for message contents'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%
				if (rs.getString("title").equals(""))
				{
%>
untitled
<%
				}
				else
				{
%>
<%= rs.getString("title") %>
<%
				}
%>
</a>
</nobr>
</td>

<td class="content">
<%= rs.getString("first_name") %> <%= rs.getString("last_name") %>
</td>

<%
				java.util.Date creationDate = rs.getDate("creation_date");
				GregorianCalendar calendar = new GregorianCalendar();
				calendar.setTime(creationDate);
%>
<td class="content">
<%= Tools.printDate(calendar) %>
</td>

<td class="content">
<a
	href="showMess.do?messageid=<%= rs.getString("message_id") %>"
	onMouseOver="windowStatus('List replies'); return true;"
	onMouseOut="windowStatus(''); return true;">
<nobr>List replies</nobr>
</a>

<%
				if (rs.getString("user_id").equals(id.getUserID()))
				{
%>
<br />
<a
	href="editMess1.do?messageid=<%= rs.getString("message_id") %>"
	onMouseOver="windowStatus('Edit message'); return true;"
	onMouseOut="windowStatus(''); return true;">
Edit message
</a>
</td>

<%
				}
%>

</tr>

<%
			}
			while (rs.next());
%>

</table>

<%
		}
		if (!data.isMainTopic())
		{
%>
<br /><br />
<a
<%
			if (data.getParentIsMainTopic())
			{
%>
	href="listMess2.do?idx=<%= data.getGrandparentIdx() %>"
<%
			}
			else
			{
%>
	href="showMess.do?messageid=<%= data.getGrandparentID() %>"
<%
			}
%>
	onMouseOver="windowStatus('Go up one level'); return true;"
	onMouseOut="windowStatus(''); return true;">
Up
</a>
<%
		}
%>
<!--
<br /><br />
<a
	href="javascript:history.go(-1);"
	onMouseOver="windowStatus('Go up one level'); return true;"
	onMouseOut="windowStatus(''); return true;">
Up
</a>-->
<%
	}
	else
	{
%>

FUCK!

<%
	}
%>
</td>
</table>

</body>

</html>