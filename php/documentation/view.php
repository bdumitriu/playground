<html>

<head>
<title>Bogdan's Documentation Database</title>
</head>

<body bgcolor="#000000">

<table border=0>

<?php
	mysql_connect("localhost", "root", "aaaaaa") or die("Couldn't connect to mysqld.");
	mysql_select_db("doc") or die ("Couldn't open 'doc' database.");
	
	$result = mysql_query("select concat(category, '/', sub_category) as 'Category',
				      title as 'Document title',
				      author as 'Author(s) or Publishing House',
				      size_mb as 'Size (Mb)',
				      format as 'Document format',
				      cd as 'On cd #'
			       from documentation d, categories c, sub_categories sc
			       where d.id_sc = sc.id_sc and sc.id_c = c.id_c
			       order by 'Category', 'Document title', 'Author(s) or Publishing House', 'On cd #'") or die("Couldn't get data.");
	
	echo '<tr bgcolor="#d3dce3">';
	echo "\n";
	for ($i = 0; $i < mysql_num_fields($result); $i++)
	{
		$name = mysql_field_name($result, $i);
		echo '<td align="center"><font face="trebuchet ms" size="2"><b>';
		echo $name;
		echo '</b></font></td>';
		echo "\n";
	}
	echo '</tr>';
	echo "\n\n";
	
	$i = 0;
	while ($array = mysql_fetch_row($result))
	{
		if ($i == 0)
		{
			echo '<tr bgcolor="#dddddd">';
			$i = 1;
		}
		else
		{
			echo '<tr bgcolor="#cccccc">';
			$i = 0;
		}
		echo "\n";
		foreach ($array as $value)
		{
			echo '<td><font face="trebuchet ms" size="1" color="black">';
			echo $value;
			echo '</font></td>';
			echo "\n";
		}
		echo '</tr>';
		echo "\n\n";
	}
?>

</table>

</body>

</html>