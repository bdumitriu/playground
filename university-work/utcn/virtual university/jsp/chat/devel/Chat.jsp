<%@ page language="java" import="java.sql.*, ro.utcluj.vu.chat.ChatData" %>

<%
	ChatData data = (ChatData) session.getAttribute("data");
	ResultSet rs = data.getResult();
	String path = "ro/utcluj/vu/chat/ClientApplet.class";
	String codebase = "../../jsp/chat";
%>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<html>

<head><title>ChatApplet</title></head>

<body>

<%
	if (rs.next())
	{
%>

<APPLET  CODE = "<%= path %>" CODEBASE = "<%= codebase %>" archive="chat.jar" WIDTH = "200" HEIGHT = "100">


    <PARAM NAME = "CHAT_USERNAME" VALUE ="<%= rs.getString("first_name") + " " + rs.getString("last_name") %>">
    <PARAM NAME = "CHAT_USERID" VALUE ="<%= data.getUserID() %>">
    <PARAM NAME = "CHAT_CLASSID" VALUE ="<%= data.getClassID() %>">
    <PARAM NAME = "CHAT_CLASSNAME" VALUE ="<%= rs.getString("name") %>">
    <PARAM NAME = "CHAT_ADDRESS" VALUE ="<%= data.getAddress() %>">
    <PARAM NAME = "CHAT_PORT" VALUE ="<%= data.getPort() %>">


</APPLET>

<%
	}
	else
	{
%>
Unable to open chat client.
<%
	}
%>

</body>

</html>

