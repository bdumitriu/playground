<?

require("config.php");
require("traffic-functions.php");

?>

<html>

<head>

<title>Traffic Report</title>
<link rel="stylesheet" href="csv-importer.css" type="text/css" />

<script language="JavaScript" src="calendar/ts_picker.js">

//Script by Denis Gritcyuk: tspicker@yahoo.com
//Submitted to JavaScript Kit (http://javascriptkit.com)
//Visit http://javascriptkit.com for this script

</script>

</head>

<body>

<div class="title">Traffic Report</div>

<br /><br />

<?

$screen = $_POST["screen"];

if ($screen == "")
{
?>

<form name="period" action="reports.php" method="post">

<table border="0" width="35%" align="center">

<tr>
	<td class="label" align="right">Begin date:&nbsp;</td>
	<td>
		<input class="userInput" type="text" name="startDate" />
		<a href="javascript:show_calendar('document.period.startDate', document.period.startDate.value);">
			<img src="calendar/cal.gif" width="16" height="16" border="0" alt="Click here to choose the start date/time" /></a>
	</td>
</tr>

<tr>
	<td class="label" align="right">End date:&nbsp;</td>
	<td>
		<input class="userInput" type="text" name="endDate" />
		<a href="javascript:show_calendar('document.period.endDate', document.period.endDate.value);">
			<img src="calendar/cal.gif" width="16" height="16" border="0" alt="Click here to choose the end date/time" /></a>
	</td>
</tr>

<tr>
	<td />
	<td align="left">
		<input class="button" type="submit" value="Generate report" />
	</td>
</tr>

</table>

<input type="hidden" name="screen" value="genReport" />

</form>

<?
}
else if ($screen == "genReport")
{
	$startDate = $_POST["startDate"];
	$endDate = $_POST["endDate"];

	if (validateDates($startDate, $endDate, $errorMsg))
	{
		$reportStatus = "This report was generated on <b>" . date("l, M jS, Y, H:i:s") .
						"</b> and shows a report of <b>" . substr($startDate, 0, 10) .
						"</b> - <b>" . substr($endDate, 0, 10) . "</b>.";
?>
<div class="text">
<? echo $reportStatus; ?>
</div>

<br /><br />

<?
		$error = false;
		$conn = @mysql_connect($mysqlServer, $mysqlUser, $mysqlPassword);
		if (!$conn)
		{
			$error = true;
			$errorMsg = "Could not connect to the MySQL server: " . mysql_error();
		}
	
		if (!@mysql_select_db($mysqlDBName, $conn))
		{
			$error = true;
			$errorMsg = "Can't use the <b>" . $mysqlDBName . "</b> database: " . mysql_error();
		}

		if ($error == true)
		{
?>

<div class="text">Error: <? echo $errorMsg; ?></div>

<?
		}
		else
		{
?>

<table border="0" width="50%" align="center">

<tr>
<td class="tableDataOdd">Most frequent calling accounts (top 5)</td>
<td class="tableDataOdd">

<?
			// Most frequent calling accounts (top 5)
			$sql = "SELECT ACCOUNT, COUNT(*) AS 'FREQ'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY ACCOUNT
					ORDER BY FREQ DESC, ACCOUNT ASC
					LIMIT 5";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["ACCOUNT"] . " (" . $row["FREQ"] . " calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Most frequent calling users (top 5)</td>
<td class="tableDataEven">

<?
			// Most frequent calling users (top 5)
			$sql = "SELECT AOA, COUNT(*) AS 'FREQ'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY AOA
					ORDER BY FREQ DESC, ACCOUNT ASC
					LIMIT 5";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["AOA"] . " (" . $row["FREQ"] . " calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<?
			// get total number of valid entries to use as denominator when computing
			// percentages in the following queries
			$sql = "SELECT COUNT(*) AS TOTAL
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'";
			
			$result = mysql_query($sql);

			if ($result == false)
			{
				$noValue = true;
				$noValueMsg = mysql_error();
			}
			else
			{
				$row = mysql_fetch_assoc($result);
				$total = $row["TOTAL"];
			}
?>

<tr>
<td class="tableDataOdd">Caller from</td>
<td class="tableDataOdd">

<?
			// Caller from
			$sql = "SELECT BZONE, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY BZONE
					ORDER BY PERCENTAGE DESC, BZONE ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["BZONE"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Destination to</td>
<td class="tableDataEven">

<?
			// Destination to
			$sql = "SELECT CZONE, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY CZONE
					ORDER BY PERCENTAGE DESC, CZONE ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["CZONE"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Gateway used</td>
<td class="tableDataOdd">

<?
			// Gateway used
			$sql = "SELECT ADA, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY ADA
					ORDER BY PERCENTAGE DESC, ADA ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["ADA"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">APORT used</td>
<td class="tableDataEven">

<?
			// APORT used
			$sql = "SELECT APORT, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY APORT
					ORDER BY PERCENTAGE DESC, APORT ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["APORT"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">BPORT used</td>
<td class="tableDataOdd">

<?
			// BPORT used
			$sql = "SELECT BPORT, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY BPORT
					ORDER BY PERCENTAGE DESC, BPORT ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["BPORT"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">CPORT used</td>
<td class="tableDataEven">

<?
			// CPORT used
			$sql = "SELECT CPORT, (COUNT(*)/$total)*100 AS 'PERCENTAGE'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY CPORT
					ORDER BY PERCENTAGE DESC, CPORT ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["CPORT"] . " (" . $row["PERCENTAGE"] . "% calls)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">BCarrier used</td>
<td class="tableDataOdd">

<?
			// BCarrier used
			$sql = "SELECT BCARRIER, (COUNT(*)/$total)*100 AS 'PERCENTAGE', SUM(BDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY BCARRIER
					ORDER BY PERCENTAGE DESC, BCARRIER ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["BCARRIER"] . " (" . $row["PERCENTAGE"] . "% calls, ". $row["SECONDS"] . " seconds)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">CCarrier used</td>
<td class="tableDataEven">

<?
			// CCarrier used
			$sql = "SELECT CCARRIER, (COUNT(*)/$total)*100 AS 'PERCENTAGE', SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
					GROUP BY CCARRIER
					ORDER BY PERCENTAGE DESC, CCARRIER ASC";
	
			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else if ($noValue == true)
			{
				echo "MySQL error: " . $noValueMsg;
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["CCARRIER"] . " (" . $row["PERCENTAGE"] . "% calls, ". $row["SECONDS"] . " seconds)";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total Premier and Budget callback duration</td>
<td class="tableDataOdd">

<?
			// Total Premier and Budget callback duration
			$sql = "SELECT SUM(BDUR)+SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo $row["SECONDS"] . " seconds";
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total Premier and Budget charges</td>
<td class="tableDataEven">

<?
			// Total Premier and Budget charges
			$sql = "SELECT SUM(BTOTCHG)+SUM(CTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					echo "$" . $row["CHARGES"];
					echo "<br />";
				}
			}
?>

</td>
</tr>

<?
			$_peakStart = strftime("%H:%M:%S", strtotime($peakStart));
			$_peakEnd = strftime("%H:%M:%S", strtotime($peakEnd));
			$_offPeakStart = strftime("%H:%M:%S", strtotime($offPeakStart));
			$_offPeakEnd = strftime("%H:%M:%S", strtotime($offPeakEnd));

			$_premier = "(";
			foreach ($premier as $value)
			{
				$_premier .= $value . ",";
			}
			$_premier[strlen($_premier)-1] = ")";

			$_budget = "(";
			foreach ($budget as $value)
			{
				$_budget .= $value . ",";
			}
			$_budget[strlen($_budget)-1] = ")";

			/*
			echo $_premier;
			echo "<br />";
			echo $_budget;
			echo "<br />";
			echo $peakStart;
			echo "<br />";
			echo $peakEnd;
			echo "<br />";
			echo $offPeakStart;
			echo "<br />";
			echo $offPeakEnd;
			echo "<br />";
			*/
?>

</table>

<br />

<table border="0" width="50%" align="center">
<caption align="top"><b>Premier</b></caption>

<tr>
<td class="tableDataOdd">Total A DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total A DUR (Peak)
			$sql = "SELECT SUM(ADUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(ACONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(ACONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total A DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total A DUR (Off-Peak)
			$sql = "SELECT SUM(ADUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(ACONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(ACONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total A Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total A Charges (Peak)
			$sql = "SELECT SUM(ATOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(ACONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(ACONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total A Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total A Charges (Off-Peak)
			$sql = "SELECT SUM(ATOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(ACONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(ACONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total B DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total B DUR (Peak)
			$sql = "SELECT SUM(BDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(BCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(BCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total B DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total B DUR (Off-Peak)
			$sql = "SELECT SUM(BDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(BCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(BCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total B Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total B Charges (Peak)
			$sql = "SELECT SUM(BTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(BCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(BCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total B Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total B Charges (Off-Peak)
			$sql = "SELECT SUM(BTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(BCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(BCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total C DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total C DUR (Peak)
			$sql = "SELECT SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(CCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(CCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total C DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total C DUR (Off-Peak)
			$sql = "SELECT SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(CCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(CCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total C Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total C Charges (Peak)
			$sql = "SELECT SUM(CTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(CCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(CCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total C Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total C Charges (Off-Peak)
			$sql = "SELECT SUM(CTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(CCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(CCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_premier";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

</table>

<br />

<table border="0" width="50%" align="center">
<caption align="top"><b>Budget</b></caption>

<tr>
<td class="tableDataOdd">Total A DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total A DUR (Peak)
			$sql = "SELECT SUM(ADUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(ACONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(ACONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total A DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total A DUR (Off-Peak)
			$sql = "SELECT SUM(ADUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(ACONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(ACONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total A Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total A Charges (Peak)
			$sql = "SELECT SUM(ATOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(ACONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(ACONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total A Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total A Charges (Off-Peak)
			$sql = "SELECT SUM(ATOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(ACONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(ACONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total B DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total B DUR (Peak)
			$sql = "SELECT SUM(BDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(BCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(BCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total B DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total B DUR (Off-Peak)
			$sql = "SELECT SUM(BDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(BCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(BCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total B Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total B Charges (Peak)
			$sql = "SELECT SUM(BTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(BCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(BCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total B Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total B Charges (Off-Peak)
			$sql = "SELECT SUM(BTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(BCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(BCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total C DUR (Peak)</td>
<td class="tableDataOdd">

<?
			// Total C DUR (Peak)
			$sql = "SELECT SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(CCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(CCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total C DUR (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total C DUR (Off-Peak)
			$sql = "SELECT SUM(CDUR) AS 'SECONDS'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(CCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(CCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$sec = $row['SECONDS'];
					if (is_null($sec))
					{
						echo "0 seconds | 0:00 minutes";
					}
					else
					{
						$secs = $sec % 60;
						if ($secs < 10)
						{
							$secs = "0" . $secs;
						}
						echo $sec . " seconds | " . floor($sec / 60) . ":" . $secs . " minutes";
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataOdd">Total C Charges (Peak)</td>
<td class="tableDataOdd">

<?
			// Total C Charges (Peak)
			$sql = "SELECT SUM(CTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND date_format(CCONDATE, '%H:%i:%s') >= '$_peakStart'
						AND date_format(CCONDATE, '%H:%i:%s') < '$_peakEnd'
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

<tr>
<td class="tableDataEven">Total C Charges (Off-Peak)</td>
<td class="tableDataEven">

<?
			// Total C Charges (Off-Peak)
			$sql = "SELECT SUM(CTOTCHG) AS 'CHARGES'
					FROM traffic
					WHERE '$startDate' <= ADETDATE AND ADETDATE <= '$endDate'
						AND BCONDATE <> '1900-01-01 00:00:00' AND CCONDATE <> '1900-01-01 00:00:00'
						AND (date_format(CCONDATE, '%H:%i:%s') >= '$_offPeakStart'
						OR date_format(CCONDATE, '%H:%i:%s') < '$_offPeakEnd')
						AND ADA IN $_budget";

			$result = mysql_query($sql);

			if ($result == false)
			{
				echo "MySQL error: " . mysql_error();
			}
			else
			{
				while ($row = mysql_fetch_assoc($result))
				{
					$chg = $row['CHARGES'];
					if (is_null($chg))
					{
						echo "$0.00000";
					}
					else
					{
						echo "$" . $chg;
					}
					echo "<br />";
				}
			}
?>

</td>
</tr>

</table>

<?
			mysql_close($conn);
?>

<br /><br />

<div class="text">
<? echo $reportStatus; ?>
</div>

<?
		}
	}
	else
	{
?>

<div class="text">Error: <? echo $errorMsg; ?></div>

<?
	}
}
?>

</body>

</html>
