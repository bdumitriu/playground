<html>
<body bgcolor="#ffffff">
<?php
	printf("<h1>Per Session Data</h1>\n");
	#phpinfo();
	if (strstr($HTTP_USER_AGENT,'Lynx'))
	{
?>
		<center>You are using the Lynx browser.</center>
<?
	}
	else
	{
?>
		<center>You are not using the Lynx browser.</center>
<?
	}
?>
</body>
</html>
