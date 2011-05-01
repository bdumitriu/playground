<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.email.EmailData" %>



<%
	EmailData data = (EmailData) session.getAttribute("data");
	String selfEmail = data.getSelfEmail();
	int index = 0;
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">

<td class="container">

<%
	ResultSet rs = data.getResult();
	if (!rs.next())
	{
%>

<h2>No e-mail addresses available at this time.</h2>

<%
	}
	else
	{
		ArrayList allEmails = new ArrayList();
		String emailAddress;

		rs.last();
		int rowCount = rs.getRow();

		rs.first();


		if (rs != null)
		{
%>

<h1>Send e-mails to:</h1>

<form	name="formname" action="multipleEmailsSelect.do?sender=<%= selfEmail %>" method="post">

<table>

<tr>
<td align="left" colspan="2">
<br />
<a	href="#"
	onClick="checkAll(emails, <%= rowCount-1 %>);">
Check all
</a>
<a	href="#"
	onClick="uncheckAll(emails, <%= rowCount-1 %>);"
	style="padding-left: 10px">
Uncheck all
</a>
<br />
<br />
</td>
</tr>

<%
			do
			{
				if ((emailAddress = rs.getString("e_mail")) != null)
				{
					if (!emailAddress.equals(selfEmail))
					{
						allEmails.add(index, emailAddress);
%>

<tr>

<td align="left">
<input
	type="checkbox"
	name="emails"
	value="<%= index %>"
/></td>

<td align="left">
<nobr> <font color="blue"> <%= rs.getString("first_name") + " " +
					rs.getString("last_name") %> </font>
</nobr>

(<%= emailAddress %>)

</td>

</tr>

<%
						index ++;
					}
				}
			} while (rs.next());

			session.setAttribute("allEmails", allEmails);
		}
	}
%>

<tr>
<td align="left" colspan="2">
<br />
<a href="#" onClick="checkAll(emails, <%= index %>);">Check all</a>
<a href="#" onClick="uncheckAll(emails, <%= index %>);" style="padding-left: 10px">Uncheck all</a>
</td>
</tr>

<tr>
<td align="center" colspan="2">
<br />
<input class="custom" type="reset" value=" << Reset >> "></input>
<input class="custom" type="submit" value=" << Send >> "></input>
</td>
</tr>

</table>

</form>

</td>
</table>

</body>

</html>