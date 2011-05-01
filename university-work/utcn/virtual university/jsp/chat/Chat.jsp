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

<!--"CONVERTED_APPLET"-->
<!-- HTML CONVERTER -->
<SCRIPT LANGUAGE="JavaScript"><!--
    var _info = navigator.userAgent;
    var _ns = false;
    var _ns6 = false;
    var _ie = (_info.indexOf("MSIE") > 0 && _info.indexOf("Win") > 0 && _info.indexOf("Windows 3.1") < 0);
//--></SCRIPT>
    <COMMENT>
        <SCRIPT LANGUAGE="JavaScript1.1"><!--
        var _ns = (navigator.appName.indexOf("Netscape") >= 0 && ((_info.indexOf("Win") > 0 && _info.indexOf("Win16") < 0 && java.lang.System.getProperty("os.version").indexOf("3.5") < 0) || (_info.indexOf("Sun") > 0) || (_info.indexOf("Linux") > 0) || (_info.indexOf("AIX") > 0) || (_info.indexOf("OS/2") > 0)));
        var _ns6 = ((_ns == true) && (_info.indexOf("Mozilla/5") >= 0));
//--></SCRIPT>
    </COMMENT>

<SCRIPT LANGUAGE="JavaScript"><!--
    if (_ie == true) document.writeln('<OBJECT classid="clsid:CAFEEFAC-0014-0000-0000-ABCDEFFEDCBA" WIDTH = "200" HEIGHT = "100"  codebase="http://java.sun.com/products/plugin/autodl/jinstall-1_4_0-win.cab#Version=1,4,0,0"><NOEMBED><XMP>');
    else if (_ns == true && _ns6 == false) document.writeln('<EMBED' +
	    'type="application/x-java-applet;jpi-version=1.4"' +
            'CODE = "<%= path %>"' +
            'CODEBASE = "<%= codebase %>"' +
            'ARCHIVE = "chat.jar"' +
            'WIDTH = "200"' +
            'HEIGHT = "100"' +
            'CHAT_USERNAME = "<%= rs.getString("first_name") + " " + rs.getString("last_name") %>"' +
            'CHAT_USERID = "<%= data.getUserID() %>"' +
            'CHAT_CLASSID = "<%= data.getClassID() %>"' +
            'CHAT_CLASSNAME = "<%= rs.getString("name") %>"' +
            'CHAT_ADDRESS = "<%= data.getAddress() %>"' +
            'CHAT_PORT = "<%= data.getPort() %>"' +
	    'scriptable=false' +
	    'pluginspage="http://java.sun.com/products/plugin/index.html#download"><NOEMBED><XMP>');
//--></SCRIPT>
<APPLET  CODE = "<%= path %>" CODEBASE = "<%= codebase %>" ARCHIVE = "chat.jar" WIDTH = "200" HEIGHT = "100"></XMP>
    <PARAM NAME = CODE VALUE = "<%= path %>" >
<PARAM NAME = CODEBASE VALUE = "<%= codebase %>" >
<PARAM NAME = ARCHIVE VALUE = "chat.jar" >

    <PARAM NAME="type" VALUE="application/x-java-applet;jpi-version=1.4">
    <PARAM NAME="scriptable" VALUE="false">
    <PARAM NAME = "CHAT_USERNAME" VALUE  ="<%= rs.getString("first_name") + " " + rs.getString("last_name") %>">
    <PARAM NAME = "CHAT_USERID" VALUE  ="<%= data.getUserID() %>">
    <PARAM NAME = "CHAT_CLASSID" VALUE  ="<%= data.getClassID() %>">
    <PARAM NAME = "CHAT_CLASSNAME" VALUE  ="<%= rs.getString("name") %>">
    <PARAM NAME = "CHAT_ADDRESS" VALUE  ="<%= data.getAddress() %>">
    <PARAM NAME = "CHAT_PORT" VALUE  ="<%= data.getPort() %>">


</APPLET>
</NOEMBED>
</EMBED>
</OBJECT>

<!--
<APPLET CODE = "<%= path %>" CODEBASE = "<%= codebase %>" ARCHIVE = "chat.jar" WIDTH = "200" HEIGHT = "100">
<PARAM NAME = "CHAT_USERNAME" VALUE  ="<%= rs.getString("first_name") + " " + rs.getString("last_name") %>">
<PARAM NAME = "CHAT_USERID" VALUE  ="<%= data.getUserID() %>">
<PARAM NAME = "CHAT_CLASSID" VALUE  ="<%= data.getClassID() %>">
<PARAM NAME = "CHAT_CLASSNAME" VALUE  ="<%= rs.getString("name") %>">
<PARAM NAME = "CHAT_ADDRESS" VALUE  ="<%= data.getAddress() %>">
<PARAM NAME = "CHAT_PORT" VALUE  ="<%= data.getPort() %>">


</APPLET>
-->
<!--"END_CONVERTED_APPLET"-->


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

