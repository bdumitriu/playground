<html>
<body>

<?php
	$link = mysql_connect("localhost", "bdumitriu", "aaaaaa") or die("could not connect");
	echo "connected successfully";
	echo "<br>";
	$result = mysql_list_dbs();
	$i = 0;
	while ($i < mysql_num_rows($result))
	{
		echo mysql_tablename($result, $i) . "<br>";
		$i++;
	}
?>

<br><br>

<?php
	/* do a select */
	mysql_select_db("test");
	$result = mysql_query("select * from users") or die("invalid query");
	while ($array = mysql_fetch_row($result))
	{
		foreach ($array as $value)
		{
			echo "$value" . "\t";
		}
		echo "<br>";
	}
	mysql_free_result($result);
	
	/* do an insert */
	mysql_query('insert into users values("bdumitriu", password("password"), "user", "Bogdan", "DUMITRIU", "bdumitriu@email.ro", "064-440058", "Str. Malinului, nr.11")');
	echo mysql_affected_rows();
	echo "<br>";
	echo mysql_error();
	mysql_close($link);
?>

</body>
</html>