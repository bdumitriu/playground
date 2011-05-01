<?php

$link = pg_connect("host=localhost dbname=dosdb user=dosuser password=dospassword")
   or die('Unable to connect.');

$query = 'SELECT * FROM Appointments';

$result = pg_query($link, $query)
   or die("Couldn't run query: " . $query);

?>

<html>

<body>

<table border="1">

<tr>
<td bgcolor="lightblue" align="center"><b>id</b></td>
<td bgcolor="lightblue" align="center"><b>start time</b></td>
<td bgcolor="lightblue" align="center"><b>end time</b></td>
<td bgcolor="lightblue" align="center"><b>description</b></td>
<td bgcolor="lightblue" align="center"><b>location</b></td>
<td bgcolor="lightblue" align="center"><b>is group appointment</b></td>
</tr>

<?php

while ($line =  pg_fetch_row($result))
{
	echo "<tr>\n";
	foreach ($line as $col_value)
	{
		$contents = str_replace("\n", "<br />", stripslashes($col_value));
		if ($contents == '')
			$contents = "N/A";
		echo "<td bgcolor=\"lightgreen\">" . $contents  . "</td>\n";
	}
	echo "</tr>\n";
}
echo "\n</table>\n";

pg_free_result($result);

pg_close($link);

?>

</body>

</html>
