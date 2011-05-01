<%@ page language="java" import="java.sql.*, java.util.*, ro.utcluj.vu.conferences.MessageData,
				 ro.utcluj.vu.main.IdentificationInfo" %>

<%
	MessageData data = (MessageData) session.getAttribute("data");
	Object tmp = session.getAttribute("id");
	IdentificationInfo id = null;
	if ((tmp != null) && (tmp instanceof IdentificationInfo))
	{
		id = (IdentificationInfo) tmp;
	}
	else
	{
		id = new IdentificationInfo();
	}
%>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<link rel="stylesheet" href="../../style/main_menu.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="menu">
<td class="menu">

<%
	ResultSet rs = data.getResult();

	if (rs != null)
	{
		int i = 1;
		if (!rs.next())
		{
%>
<center>
<h2><font color="white">
<br />No<br />conferences<br />available<br />at<br />this<br />time.
</font></h2>
</center>
<%
		}
		else
		{
%>
<br /><br />
<table border="0">
<%
			do
			{
%>

<tr>

<td>
<%
				if (rs.getBoolean("newMessagesFlag"))
				{
%>
<img src="../../img/r_bullet.jpg" />
<%
				}
				else
				{
%>
<img src="../../img/g_bullet.jpg" />
<%
				}
%>
</td>

<td>
<nobr>
<a	class="simple"
	href="listMess2.do?idx=<%= rs.getInt("idx") %>"
	target="main"
	onMouseOver="windowStatus('List main topics'); return true;"
	onMouseOut="windowStatus(''); return true;">
<%
				if (rs.getString("name").equals(""))
				{
%>
untitled
<%
				}
				else
				{
%>
<%= rs.getString("name") %>
<%
				}
%>
</a>
</nobr>
</td>

</tr>
<tr>

<td></td>

<td>
<nobr>
<font color="white">
(&nbsp;<a	class="small"
	target="main"
	href="displayConf.do?idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Display conference data'); return true;"
	onMouseOut="windowStatus(''); return true;">
view description
</a>&nbsp;)
<%
				if ((id.getPosition().equals("teacher"))
					|| (rs.getBoolean("mainTopicsFlag")))
				{
%>
<br />
(&nbsp;<a	class="small"
	target="main"
	href="../../jsp/conferences/AddMainTopicData.jsp?idx=<%= rs.getInt("idx") %>"
	onMouseOver="windowStatus('Add main topic'); return true;"
	onMouseOut="windowStatus(''); return true;">
add main topic
</a>&nbsp;)
<%
				}
%>
</font>
</nobr>
</td>

</tr>

<%
			}
			while (rs.next());
%>
</table>

<br />

<table border="0">

<tr>
<td><img src="../../img/r_bullet.jpg" /></td>
<td><font color="white">unread message(s).</font></td>
</tr>

<tr>
<td><img src="../../img/g_bullet.jpg" /></td>
<td><font color="white">read message(s).</font></td>
</tr>

</table>
<%
		}
	}
%>

<table border="0">

<tr>
<td colspan="2" align="center">
<br />
<a	class="menu"
	target="_parent"
	href="../../html/<%= id.getPosition() %>/submenu_conf.html"
	onMouseOver="windowStatus('Back to main menu'); return true;"
	onMouseOut="windowStatus(''); return true;"
>Back To Main Menu</a>
</td>
</tr>

</table>

</td>
</table>

</body>

</html>