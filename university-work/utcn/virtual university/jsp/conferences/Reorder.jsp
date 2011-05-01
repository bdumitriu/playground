<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.conferences.ConferenceData,
				 ro.utcluj.vu.utils.Tools" %>

<%
	ConferenceData data = (ConferenceData) session.getAttribute("data");
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/conferences/reorder.js"></script>

<script language="javaScript">
<%
	ResultSet rs = data.getResult();
	if (rs != null)
	{
		rs.last();
		int rowCount = rs.getRow();
		rs.beforeFirst();
%>
	var nr = <%= rowCount %>;
</script>

<body>

<table class="container">
<td class="container">

<%
		if (!rs.next())
		{
%>
<h2>No conferences available at this time.</h2>
<%
		}
		else
		{
%>

<h1>Reorder conferences</h1>

<table border="0">
<form action="reorderConf2.do" method="post" name="form">
<tr>

<td align="center">
<table border="0">
<%
			int i = 0;
			do
			{
				i++;
%>

<tr>
<td align="center">
<%
				if (i == 1)
				{
%>
<input type="radio" name="conf" value="r<%= i %>" checked />
<%
				}
				else
				{
%>
<input type="radio" name="conf" value="r<%= i %>" />
<%
				}
%>
<input class="custom" type="text" name="c<%= i %>" value="<%= Tools.replaceHTMLChars(rs.getString("name")) %>" readonly />
<input type="hidden" name="pos<%= i %>" value="<%= rs.getInt("idx") %>" readonly />
</td>
</tr>

<%
			}
			while (rs.next());
%>

</table>
</td>

<td align="center" valign="middle">
<table border="0">

<tr>
<td align="center">
<input class="custom" type="button" value=" << up >> " onClick="moveUp();" />
</td>
</tr>

<tr>
<td align="center">
<input class="custom" type="button" value="<< down >>" onClick="moveDown();" />
</td>
</tr>

</table>
</td>

</tr>

<tr>
<td align="center" colspan="2">
<input class="custom" type="submit" value=" << Save order >> " />
<input type="hidden" name="conferences" value="<%= rowCount %>" />
</td>
</tr>

</form>
</table>

<%
		}
%>

</td>
</table>

</body>

<%
	}
%>

</html>