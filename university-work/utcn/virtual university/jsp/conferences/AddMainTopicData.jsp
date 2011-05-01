<%@ page language="java" %>

<html>

<link rel="stylesheet" href="../../style/vu_main.css" type="text/css" />
<script language="javaScript" src="../../js/utils.js"></script>

<body>

<table class="container">
<td class="container">

<h1>
Add a new message
</h1>

<table border="0">

<form	action="addMess.do?idx=<%= request.getParameter("idx") %>"
		method="post">
<tr>

<td align="right">Message title:</td>
<td align="left"><input class="custom" type="text" name="title" maxlength="50"></input></td>
</tr>

<tr>
<td align="right">Message text:</td>
<td align="left"><br /><textarea class="custom" name="contents" rows="5" cols="40"></textarea></td>
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