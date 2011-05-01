<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.email.EmailData" %>

<%
	EmailData data = (EmailData) session.getAttribute("data");
	String selfEmail = data.getSelfEmail();
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">


<%
	ResultSet rs = data.getResult();
	String emailAddress;

	if (!rs.next())
	{
%>

<h2>No e-mail addresses available at this time.</h2>


<%
	}
	else
	{
		if (rs != null)
		{
%>
<h1>Send e-mail to:</h1>

<table>
<%
			do
			{
				if ((emailAddress = rs.getString("e_mail")) != null)
				{
					if (!emailAddress.equals(selfEmail))
					{
%>
<tr align="left">
<nobr>
<a
	href="../../jsp/email/EditEmail.jsp?user_ID=<%= rs.getString("user_ID") %>&rcpt=<%= emailAddress %>&self=<%= selfEmail %>"
	onMouseOver="windowStatus('Edit e-mail'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%= rs.getString("first_name") + " " + rs.getString("last_name") %>
</a>
&nbsp(<%= emailAddress %>)
</nobr>
<br />
<br />
</tr>
<%
					}
				}
			} while (rs.next());
		}
	}
%>
</table>
</td>
</table>

</body>

</html>