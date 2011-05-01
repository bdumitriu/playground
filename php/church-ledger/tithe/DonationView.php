<?php
/*******************************************************************************
 *
 *  filename    : DonationView.php
 *  last change : 2003-03-14
 *  description : Displays a list of all a person's donation for this year
 *
 *  http://osc.sourceforge.net
 * 
 *  This product is based upon work previously done by Infocentral (infocentral.org)
 *  on their PHP version Church Management Software that they discontinued
 *  and we have taken over.  We continue to improve and build upon this product
 *  in the direction of excellence.
 * 
 *  OpenSourceChurch (OSC) is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  Any changes to the software must be submitted back to the OpenSourceChurch project
 *  for review and possible inclusion.
 *
 *  Copyright 2003  Michael Slemen, Chris Gebhardt
 ******************************************************************************/

//Include the function library
require "Include/Config.php";
require "Include/Functions.php";

if (!$_SESSION['bFinance'])
{
	Redirect("Menu.php");
	exit;
}

//Set the page title
$sPageTitle = gettext("View Donations");
require "Include/Header.php";

if (isset($_GET["Year"]))
{
	$year = FilterInput($_GET["Year"],'int');
}
else
{
	$today = getdate();
	$year = $today['year'];
}

$iPersonID = FilterInput($_GET["PersonID"],'int');
$sNoDetail = FilterInput($_GET["NoDetail"],'int');

$showTithe = true;

if ($iPersonID > 0)
{
	$sSQL = "SELECT per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix
			FROM person_per
			WHERE per_ID=" . $iPersonID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];

	$rsPers = RunQuery($sSQL);
	if (mysql_num_rows($rsPers) > 0)
	{
		$aRow = mysql_fetch_array($rsPers);
		$sFullName = "";
		if ($aRow['per_Title'] != "") { $sFullName .= $aRow['per_Title'] . " "; }
		if ($aRow['per_FirstName'] != "") { $sFullName .= $aRow['per_FirstName'] . " "; }
		if ($aRow['per_MiddleName'] != "") { $sFullName .= $aRow['per_MiddleName'] . " "; }
		if ($aRow['per_LastName'] != "") { $sFullName .= $aRow['per_LastName'] . " "; }
		if ($aRow['per_Suffix'] != "") { $sFullName .= $aRow['per_Suffix'] . " "; }
	}

	//$sSQL = "SELECT SUM(dna_Amount) AS tithe
	//		FROM donations_don, donationamounts_dna
	//		WHERE don_DonorID=$iPersonID AND donations_don.don_ID = donationamounts_dna.dna_don_ID AND don_Date >= '$year-1-1' AND don_Date <= '$year-12-31' AND donations_don.chu_Church_ID=" . $_SESSION['iChurchID'];

	$sSQL = "SELECT tth_Amount AS tithe
			FROM tithe_tth
			WHERE per_person_ID=" . $iPersonID . " AND tth_Year=" . $year . " AND chu_Church_ID=" . $_SESSION['iChurchID'];

	$result = RunQuery($sSQL);
	if (mysql_num_rows($result) > 0)
	{
		$aRow = mysql_fetch_array($result);
		$sTithe = $aRow['tithe'];
	}
	else
	{
		$sTithe = "0";
	}
}
else
{
	$showTithe = false;
}



$sSQL = "SELECT per_FirstName as FirstName, per_MiddleName as MiddleName, per_LastName as LastName,
			don_ID as donID, don_PaymentType as type, don_CheckNumber as checkno,
			SUM(dna_Amount) as amount, don_Date as date
		FROM donations_don
		LEFT JOIN person_per ON donations_don.don_DonorID = person_per.per_ID
		LEFT JOIN donationamounts_dna ON donations_don.don_ID = donationamounts_dna.dna_don_ID
		WHERE don_Date >= '$year-1-1' AND don_Date <= '$year-12-31' AND donations_don.chu_Church_ID=" . $_SESSION['iChurchID'] ;

if ($iPersonID > 0) $sSQL .= " AND don_DonorID=$iPersonID ";

$sSQL .= "GROUP BY don_ID ORDER BY don_Date Desc" ;

$result = RunQuery($sSQL);

$numRows = mysql_num_rows($result);

if ($iPersonID > 0)
{
	echo "<p class=\"LargeText\"><b>" . gettext("Year") . ": </b>";
	echo "<a href=\"DonationView.php?PersonID=$iPersonID&NoDetail=$sNoDetail" . "&Year=" . ($year - 1) . "\"><img src=\"Images/downarrow.gif\" border=\"0\"></a>";
	echo $year;
	echo "<a href=\"DonationView.php?PersonID=$iPersonID&NoDetail=$sNoDetail" . "&Year=" . ($year + 1) . "\"><img src=\"Images/uparrow.gif\" border=\"0\"></a>";
	echo "</p>";

	echo "[ <a href=\"PersonView.php?PersonID=$iPersonID\">" . gettext("Back to Person Record") . "</a> ] ";
	if ($numRows >= 1)
	{
		echo "[ <a href=\"Reports/DonationReportYearly.php?PersonID=$iPersonID&year=$year\" target=\"exemption\">" . gettext("View") . " $year " . gettext("exemption letter") . "</a> ] ";
		if ($sNoDetail)
		{
			echo "[ <a href=\"DonationView.php?PersonID=$iPersonID&Year=$year\">" . gettext("View Details") . "</a> ] ";
		}
		else
		{
			echo "[ <a href=\"DonationView.php?NoDetail=1&Year=$year&PersonID=$iPersonID\">" . gettext("Hide Details") . "</a> ] ";
		}

	}
	echo "[ <a href=\"DonationEditor.php?PersonID=$iPersonID\">" . gettext("Add a new donation") . "</a> ] ";
	echo "[ <a href=\"DonationMove.php?From=" . $iPersonID . "\">" . gettext("Move All Donations") . "</a> ] ";
	echo "[ <a href=\"Reports/TitheReport.php?year=" . $year . "&PersonID=" . $iPersonID . "\">" . gettext("Tithe Report") . "</a> ] ";
	echo "[ <a href=\"TitheEntry.php?CheckTithe=&TitheYear=" . $year . "&PersonList=" . $iPersonID . "\">" . gettext("Add/Modify Tithe") . "</a> ]<br><br>";
}

if ($numRows >= 1) { ?>
	<table cellpadding="4" align="center" cellspacing="0" width="90%">
	<tr class="TableHeader">
		<td><?php echo gettext("Name"); ?></td>
		<td><?php echo gettext("Amount"); ?></td>
		<td><?php echo gettext("Date"); ?></td>
		<td><?php echo gettext("Type"); ?></td>
		<td><?php echo gettext("Check No"); ?></td>
		<td><?php echo gettext("Receipt"); ?></td>
		<td><?php echo gettext("Options"); ?></td>
	</tr>

<?php
	while ($row = mysql_fetch_array($result))
	{
		extract($row) ;
		echo "<tr>" ;
		echo "<td>$FirstName $MiddleName $LastName</td>" ;
		echo "<td>" . FormatNumber($amount,'money') . "</td>" ;
		echo "<td>$date</td>" ;
		if ($type==1) $type = gettext("cash");
		if ($type==2) $type = gettext("check");
		if ($type==3) $type = gettext("credit");
		echo "<td>$type</td>" ;
		if ($checkno==0) $checkno = "" ;
		echo "<td>$checkno</td>" ;
		echo "<td>$donID</td>" ;
		echo "<td>
			<a href=\"DonationEditor.php?DonationID=$donID\">" . gettext("Edit") . "</a> |
			<a href=\"DonationDelete.php?PersonID=$iPersonID&donID=$donID\">" . gettext("Delete") . "</a> |
			<a href=\"DonationViewReceipt.php?Receipt=$donID\" target=\"receipt\">" . gettext("Receipt") . "</a>
			</td>" ;
		echo "</tr>" ;
		if (!$sNoDetail)
		{
			$firstRow = true;
			$sSQL = "SELECT fun_name AS Fund, dna_amount AS Amount FROM donations_don LEFT JOIN donationamounts_dna ON donations_don.don_ID = donationamounts_dna.dna_don_ID LEFT JOIN donationfund_fun ON donationamounts_dna.dna_fun_ID = donationfund_fun.fun_ID WHERE dna_don_ID = $donID ORDER BY dna_fun_ID LIMIT 0 , 30";
			$rsDonDetail = RunQuery($sSQL);
			while ($aDetails= mysql_fetch_array($rsDonDetail))
			{
				extract($aDetails);
				if ($firstRow)
				{
					echo "<tr><td colspan=\"2\"><table width=\"100%\" cellspacing=\"0\"><tr><td width=\"15%\"></td><td><table width=\"90%\" cellspacing=\"0\">";
					echo "<tr class=\"TinyTableHeader\"><td colspan=\"2\">" .  gettext("Donation by Fund") . "</td></tr>";
					$firstRow = false;
				}
				$sRowClass = AlternateRowStyle($sRowClass);
				echo "<tr class=\"$sRowClass\"><td nowrap>" . $Fund . "</td><td align=\"right\">" . FormatNumber($Amount,'money') . "</td></tr>";

			}
			if (!$firstRow) echo "</table></td></tr></table></td></tr>";
		}
	}
	echo "</table>" ;
}
?>
		<table cellpadding="4" width="90%" align="center">
			<tr class="TableHeader">
				<td><?php echo gettext("Amount tithed by ") . $sFullName . " " . gettext("in") . " " . $year . ": " . FormatNumber($sTithe,'money'); ?></td>
			</tr>
		</table>
<?php
require "Include/Footer.php";
?>
