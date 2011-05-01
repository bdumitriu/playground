<html>
<body>

<h3>Informatii despre utilizatorii paginii LT Leasing Transilvania</center>
<br><br><br>

<table border=1>

<?php
	mysql_connect("localhost", "lt", "web") or die("Nu m-am putut conecta la mysqld.");
	mysql_select_db("lt_web") or die ("N-am putut sa deschid baza de date lt_web.");
	
	$result = mysql_query("select nume as 'Numele clientului', email as 'Adresa sa de e-mail', telefon as 'Telefonul', firma as 'Firma', telfirma as 'Telefonul firmei', emailfirma as 'Adresa de e-mail a firmei', web as 'Pagina web', cifraaf as 'Cifra de afaceri', domeniu as 'Domeniul', oras as 'Orasul'
			       from clienti") or die("N-am putut sa obtin datele.");
	
	echo "<tr>";
	for ($i = 0; $i < mysql_num_fields($result); $i++)
	{
		$name = mysql_field_name($result, $i);
		echo "<td><b>$name</b></td>";
	}
	echo "</tr>";
	
	while ($array = mysql_fetch_row($result))
	{
		echo "<tr>";
		foreach ($array as $value)
		{
			echo "<td>$value</td>";
		}
		echo "</tr>";
	}
?>

</table>

</body>
<html>
