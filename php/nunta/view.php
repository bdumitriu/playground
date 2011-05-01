<?php

if (isset($_POST["Password"]))
{
	$parola = $_POST["parola"];

	if ($parola == "pussiubit")
	{

		$link = mysql_connect('localhost', 'nunta', 'nunta9iuliedb')
		   or die('Din pacate, este ceva problema. Let me know.');

		mysql_select_db('nunta')
		   or die('Din pacate, este ceva problema. Let me know.');

		$query = 'SELECT * FROM Confirmare';

		$result = mysql_query($query)
		   or die('Din pacate, este ceva problema. Let me know.');

?>

<html>

<head>
<meta http-equiv="Cache-Control" content="no-cache" />
<title>Confirmari so far</title>
<head>

<body>

<font style="font-family: verdana; font-size: 20">Confirmari so far...</font>

<br /><br />

<?php

	if (mysql_num_rows($result) == 0)
	{
		echo "<font style=\"font-family: verdana; font-size: 15\">... inca nici una :(</font>";
	}
	else
	{
?>

<table border="1">

<tr>
<td bgcolor="lightblue" align="center"><b><font style="font-family: verdana; font-size: 12; letter-spacing: 1">Nume</font></b></td>
<td bgcolor="lightblue" align="center"><b><font style="font-family: verdana; font-size: 12; letter-spacing: 1">Nr.<br />persoane</font></b></td>
<td bgcolor="lightblue" align="center"><b><font style="font-family: verdana; font-size: 12; letter-spacing: 1">Data si ora<br />confirmarii</font></b></td>
<td bgcolor="lightblue" align="center"><b><font style="font-family: verdana; font-size: 12; letter-spacing: 1">Alte cele</font></b></td>
</tr>

<?php

		while ($line = mysql_fetch_array($result, MYSQL_ASSOC))
		{
			echo "<tr>\n";
			foreach ($line as $col_value)
			{
				$contents = str_replace("\n", "<br />", stripslashes($col_value));
				if ($contents == '')
					$contents = "N/A";
				echo "<td bgcolor=\"lightgreen\"><font style=\"font-family: verdana; font-size: 12\">" . $contents  . "</font></td>\n";
			}
			echo "</tr>\n";
		}
		echo "\n</table>\n";

	}

	mysql_free_result($result);

	mysql_close($link);

?>

</body>

</html>

<?php
	}
	else
	{
?>

<html>

<head>
<meta http-equiv="Cache-Control" content="no-cache" />
<title>Confirmari so far</title>
<head>

<body>

<font style="font-family: verdana; font-size: 20; color: red">Ain't that cute? But it's wrong!!!</font>
<br /><br />
<font style="font-family: verdana; font-size: 14">
<a href="view.php">Try again?</a>
</font>

</body>

</html>

<?php		
	}
}
else
{
?>

<html>

<head>
<meta http-equiv="Cache-Control" content="no-cache" />
<title>Confirmari so far</title>
<head>

<body>

<form method="post" action="view.php">
<font style="font-family: verdana; font-size: 20">Parola: </font><input type="password" name="parola" />
<input type="submit" name="Password" value="Try me!">
</form>

</body>

</html>

<?php
}
?>
