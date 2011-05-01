<html>

<head>
<title>Bogdan's Documentation Database</title>
</head>

<body bgcolor="#cccccc">

<form method="post" action="insert.php">

<table border="0">

<tr>
<td align="right">id_sc:</td>
<td><input type="text" name="id_sc" size="10" maxlength="10"></td>
</tr>

<tr>
<td align="right">title:</td>
<td><input type="text" name="title" size="70" maxlength="70"></td>
</tr>

<tr>
<td align="right">author:</td>
<td><input type="text" name="author" size="70" maxlength="70"></td>
</tr>

<tr>
<td align="right">size_mb:</td>
<td><input type="text" name="size" size="10" maxlength="10"></td>
</tr>

<tr>
<td align="right">format:</td>
<td><input type="text" name="format" size="10" maxlength="10"></td>
</tr>

<tr>
<td align="right">on cd #:</td>
<td><input type="text" name="cd" size="10" maxlength="10"></td>
</tr>

<tr>
<td></td>
<td><input type="submit" name="submit" value="insert"></td>
</tr>

</table>

<?php
	if (isset($submit))
	{
		mysql_connect("localhost", "root", "aaaaaa") or die("Couldn't connect to mysqld.");
		mysql_select_db("doc") or die ("Couldn't open 'doc' database.");

		/*
		 * Create the query to send to mysqld based on
		 * the user values.
		 */	
		$query = "insert into documentation (";
		$values = ") values (";
		$test = 0;
		if ($id_sc != "")
		{
			$query .= "id_sc";
			$values .= "$id_sc";
			$test = 1;
		}
		if ($title != "")
			if ($test == 1)
			{
				$query .= ", title";
				$values .= ", " . "'$title'";
			}
			else
			{
				$query .= "title";
				$values .= "'$title'";
				$test = 1;
			}
		if ($author != "")
			if ($test == 1)
			{
				$query .= ", author";
				$values .= ", " . "'$author'";
			}
			else
			{
				$query .= "author";
				$values .= "'$author'";
				$test = 1;
			}
		else
		{
			$query .= ", author";
			$values .= ", " . "''";
		}
		if ($size != "")
		{
			$query .= ", size_mb";
			$values .= ", " . "$size";
		}
		if ($format != "")
		{
			$query .= ", format";
			$values .= ", " . "'$format'";
		}
		if ($cd != "")
		{
			$query .= ", cd";
			$values .= ", " . "$cd";
		}
		$query .= $values . ")";

		$result = mysql_query($query) or die("Couldn't insert data.");
		echo "Record inserted.";
	}
?>

</form>

</body>

</html>