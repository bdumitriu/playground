<%@ page language="java" import="ro.utcluj.vu.main.IdentificationInfo,
				 java.io.File,
				 ro.utcluj.vu.conferences.MessageData,
				 java.sql.ResultSet,
				 java.util.GregorianCalendar,
				 ro.utcluj.vu.utils.Tools"%>

<%
	Object tmp = session.getAttribute("data");
	MessageData data = null;
	if ((tmp != null) && (tmp instanceof MessageData))
	{
		data = (MessageData) tmp;
	}

	boolean test = false;
	String messageID = "";
	String parentID = "";
	String title = "";
	String contents = "";
	String firstName = "";
	String lastName = "";
	GregorianCalendar creationDate = new GregorianCalendar();
	byte studentInteractivity = -1;
	String idx = "";

	if (data != null)
	{
		ResultSet rs = data.getResult();
		if (rs.next())
		{
			messageID = rs.getString("message_id");
			parentID = rs.getString("parent_id");
			title = rs.getString("title");
			contents = rs.getString("message");
			firstName = rs.getString("first_name");
			lastName = rs.getString("last_name");
			studentInteractivity = rs.getByte("student_interactivity");
			creationDate.setTime(rs.getDate("creation_date"));
			idx = (String) rs.getString("idx");
		}
		else
		{
			test = true;
		}
	}
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<%
	if (test)
	{
%>

<h1>
No information found in the database for the requested message.
</h1>

<%
	}
	else
	{
%>

<h1>
Message details
</h1>

<table class="bgcolor" width="70%">

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top" width="30%">
Message title:
</td>
<td class="bgcolor2" align="left">
<%= title %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Message contents:
</td>
<td class="bgcolor2" align="left">
<%= contents %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Message author:
</td>
<td class="bgcolor2" align="left">
<%= firstName + " " + lastName %>
</td>
</tr>

<tr class="bgcolor">
<td class="bgcolor1" align="left" valign="top">
Written on:
</td>
<td class="bgcolor2" align="left">
<%= Tools.printDate(creationDate) %>
</td>
</tr>

</table>

<br /><br />
<%
		IdentificationInfo id = null;
		tmp = session.getAttribute("id");

		if ((tmp != null) && (tmp instanceof IdentificationInfo))
		{
			id = (IdentificationInfo) tmp;
		}
		else
		{
			id = new IdentificationInfo(null, null, "");
		}

		if ((id.getPosition().equals("teacher")) || (studentInteractivity == 0) ||
			(studentInteractivity == 1))
		{
%>
<a
	href="../../jsp/conferences/AddMessageData.jsp?messageid=<%= messageID %>"
	onMouseOver="windowStatus('Reply to this message'); return true;"
	onMouseOut="windowStatus(''); return true;">
Reply
</a>
<%
		}
%>

&nbsp&nbsp&nbsp&nbsp
<a
<%
			if (parentID != null)
			{
%>
	href="showMess.do?messageid=<%= parentID %>"
<%
			}
			else
			{
%>
	href="listMess2.do?idx=<%= idx %>"
<%
			}
%>
	onMouseOver="windowStatus('Go back'); return true;"
	onMouseOut="windowStatus(''); return true;">
Back
</a>

<%
	}
%>

</td>
</table>

</body>

</html>