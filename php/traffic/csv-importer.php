<?

require("config.php");
require("traffic-functions.php");
$screen = $_POST["screen"];

?>

<html>

<head>

<title>CSV-to-MySQL Importer</title>
<link rel="stylesheet" href="csv-importer.css" type="text/css" />

</head>

<body>

<div class="title">CSV-to-MySQL Import Tool</div>

<br /><br />

<?

if ($screen == "")
{
?>

<form action="csv-importer.php" method="post">

<table border="0" width="35%" align="center">

<tr>
	<td class="label" align="right">Name of CSV file (relative to script):&nbsp;</td>
	<td>
		<input class="userInput" type="text" name="csvFile" />
	</td>
</tr>

<tr>
	<td />
	<td align="left">
		<input class="button" type="submit" value="Preview >" />
	</td>
</tr>

</table>

<input type="hidden" name="screen" value="preview" />

</form>

<?
}
else if ($screen == "preview")
{
	$csvFile = $_POST["csvFile"];

	readCSVData($csvFile, $preview, $headerData, $rowData, $nrRows, $error, $errorMsg);

	if ($error == true)
	{
?>

<div class="text">Error: <? echo $errorMsg; ?></div>

<?
	}
	else
	{
?>

<form action="csv-importer.php" method="post">

<table border="0">

<tr>
<td align="left">
	<input class="button" type="submit" value="Import >" />
</td>
</tr>

<tr>
<?
		for ($i = 0; $i < $nrFields; $i++)
		{
			echo "<td class=\"tableHeader\">" . $headerData[$i] . "</td>\n";
		}
?>
</tr>

<?
		for ($i = 0; $i < $nrRows ; $i++)
		{
			echo "<tr>\n";
			for ($j = 0; $j < $nrFields; $j++)
			{
				if ($i % 2 == 0)
				{
					echo "<td class=\"tableDataEven\">" . $rowData[$i][$j] . "</td>\n";
				}
				else
				{
					echo "<td class=\"tableDataOdd\">" . $rowData[$i][$j] . "</td>\n";
				}
			}
			echo "</tr>\n";
		}
?>

<tr>
<td align="left">
	<input class="button" type="submit" value="Import >" />
</td>
</tr>

</table>

<input type="hidden" name="csvFile" value=<? echo $csvFile; ?> />
<input type="hidden" name="screen" value="import" />

</form>

<?
	}
}
else if ($screen == "import")
{
	$csvFile = $_POST["csvFile"];

	importToMySQL($csvFile, $error, $errorMsg, $duplicateEntries);

	if ($error == true)
	{
?>

<div class="text">Error: <? echo $errorMsg; ?></div>

<?
	}
	else
	{
		
?>

<div class="text">
Import of data from file "<? echo $csvFile; ?>" is complete.
<?
		$deSize = sizeof($duplicateEntries);
		if ($deSize > 0)
		{
?>
<br />
The following rows could not be inserted because they were duplicates:

<?
			for ($i = 0; $i < ($deSize-1); $i++)
			{
				echo "<b>" . ($duplicateEntries[$i]+1) . "</b>" . ", ";
			}
			echo "<b>" . ($duplicateEntries[$deSize-1]+1) . "</b>" . ".";
		}
?>
</div>

<form action="csv-importer.php" method="post">
	<input class="button" type="submit" value="Back >" />
</form>

<?
	}
}
?>

</body>

</html>
