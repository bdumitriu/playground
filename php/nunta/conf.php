<?php
	$parola = $_POST["parola"];

	$link = mysql_connect('localhost', 'nunta', 'nunta9iuliedb')
	   or die('Din pacate, este ceva problema. Anuntati-ma.');

	mysql_select_db('nunta')
	   or die('Din pacate, este ceva problema. Anuntati-ma.');

	$query = 'SELECT nume, nr_pers FROM Confirmare WHERE nume != "puss" ORDER BY date';

	$result = mysql_query($query)
	   or die('Din pacate, este ceva problema. Anuntati-ma.');
?>

<html>

<head>
<meta http-equiv="Cache-Control" content="no-cache" />
<title>Confirmari</title>
<head>

<body>

<font style="font-family: verdana; font-size: 20">Confirmari</font>

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