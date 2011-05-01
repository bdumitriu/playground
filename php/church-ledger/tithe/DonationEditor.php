<?php
/*******************************************************************************
 *
 *  filename    : DonationEditor.php
 *  last change : 2003-03-21
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
 *  Copyright 2003 Chris Gebhardt
 *
 ******************************************************************************/

//Include the function library
require "Include/Config.php";
require "Include/Functions.php";

if (!$_SESSION['bFinance'])
{
	Redirect("Menu.php");
	exit;
}

// Set number of possible fund rows in the entry form
$iFundEntries = 6;

//Set the page title
$sPageTitle = gettext("Add/Edit a Donation");

//Get the DonationID from the querystring
$iDonationID = FilterInput($_GET["DonationID"],'int');

//Get the PersonID out of the querystring.  This is used only when adding a donation.
$iPersonID = FilterInput($_GET["PersonID"],'int');

// See if we're using the batch entry mode:
if (isset($_GET["Batch"]))
{
	$batchMode = true;
	if (isset($_POST["DefaultFundID"])) $_SESSION['iDefaultFundID'] = FilterInput($_POST["DefaultFundID"],'int');
	if (isset($_POST["DefaultPaymentType"])) $_SESSION['iDefaultPaymentType'] = FilterInput($_POST["DefaultPaymentType"],'int');
	if (!empty($_POST["DefaultDate"]))
		$_SESSION['sDefaultDate'] = FilterInput($_POST["DefaultDate"],'char',10);
	elseif (!isset($_SESSION['sDefaultDate']))
	{
		$today = getdate();
		$_SESSION['sDefaultDate'] = $today['year'] . "-" . $today['mon'] . "-" . $today['mday'];
	}
}
else
	$batchMode = false;

// Security: User must have Add or Edit Records permission to use this form in those manners
// Clean error handling: (such as somebody typing an incorrect URL ?PersonID= manually)
if ($batchMode)
{
	if (!$_SESSION['bAddRecords'])
	{
		Redirect("Menu.php");
		exit;
	}
	$editorMode = 0;
}
elseif (strlen($iDonationID) >= 1)
{
	if (!$_SESSION['bEditRecords'])
	{
		Redirect("Menu.php");
		exit;
	}
	$sSQL = "SELECT don_ID FROM donations_don WHERE don_ID = " . $iDonationID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
	if (mysql_num_rows(RunQuery($sSQL)) == 0)
	{
		Redirect("Menu.php");
		exit;
	}
	$editorMode = 1;
}
elseif (strlen($iPersonID) >= 1)
{
	if (!$_SESSION['bAddRecords'])
	{
		Redirect("Menu.php");
		exit;
	}
	$sSQL = "SELECT per_ID,per_Envelope FROM person_per WHERE per_ID = " . $iPersonID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
	$rsPerson = RunQuery($sSQL);
	if (mysql_num_rows($rsPerson) == 0)
	{
		Redirect("Menu.php");
		exit;
	}
	$aRow = mysql_fetch_array($rsPerson);
	if (isset($aRow['per_Envelope'])) $iEnvelope = $aRow['per_Envelope'];

	$editorMode = 0;
}
else
{
	Redirect("Menu.php");
	exit;
}

// Is this the second pass?
if (isset($_POST["DonationSubmit"]))
{
	//Assign everything locally
	$iPaymentType = FilterInput($_POST["PaymentType"],'int');
	$iCheckNumber = FilterInput($_POST["CheckNumber"],'int');
	$iEnvelope = FilterInput($_POST["DonorList"],'int');
	$sDate = FilterInput($_POST["Date"],'char',10) ;

	if ($iPaymentType > 0) {
		// If payment is by check, validate check number, otherwise set to zero
		if ($iPaymentType != 2) {
			$iCheckNumber = 0 ;
		}
		elseif ($iCheckNumber < 1) {
			$sCheckError = gettext("You must enter a valid check number.");
			$bErrorFlag = True;
		}
	}
	else {
		$sPaymentError = gettext("You must select the payment type.");
		$bErrorFlag = True;
	}

	list($iYear, $iMonth, $iDay) = sscanf($sDate,"%04d-%02d-%02d");
	if ( !checkdate($iMonth,$iDay,$iYear) )
	{
		$sDateError = "<span style=\"color: red; \"> " . gettext("Not a valid date") . "</span>";
		$bErrorFlag = true;
	}

	if (strlen($iEnvelope) > 0)
	{
		$sSQL = "SELECT per_ID AS iPersonID FROM person_per WHERE per_Envelope = " . $iEnvelope . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
		$rsQuery = RunQuery($sSQL);

		if (mysql_num_rows($rsQuery) == 0)
		{
			$sEnvelopeError = gettext("Invalid envelope number!");
			$bErrorFlag = true;
		}
		else
			// Get the PersonID assigned this envelope: needed later for recording the donation.
			if ($batchMode) extract(mysql_fetch_array($rsQuery));
	}
	elseif ($batchMode)
	{
		$sEnvelopeError = gettext("Envelope number must always be<br>specified in batch entry mode");
		$bErrorFlag = true;
	}
	
	// Assign values to arrays and Validate the donation fund amounts
	// There must be one valid row or else throw error.
	$noValid = true;
	for ($iRow; $iRow <= $iFundEntries; $iRow++)
	{
		$aFundIDs[$iRow] = FilterInput($_POST["FundID" . $iRow],'int');
		$aFundAmounts[$iRow] = FilterInput($_POST["FundAmount" . $iRow],'float');
		if ($aFundIDs[$iRow] > 0 && $aFundAmounts[$iRow] > 0) $noValid = false;
	}
	if ($noValid) $bErrorFlag = true;

	// If no errors, then let's update...
	if (!$bErrorFlag)
	{
		// Editing old donation
		if ($editorMode == 1)
		{
			$iDonorID = FilterInput($_POST['DonorID'],'int');
			if ($iEnvelope <= 0) $iEnvelope = "NULL";
			
			// Update basic donation info.
			$sSQL = "UPDATE donations_don SET don_PaymentType=$iPaymentType, don_DonorID=$iDonorID,
					don_CheckNumber=$iCheckNumber,don_Date='$sDate',don_Envelope=$iEnvelope
					WHERE don_ID = $iDonationID AND chu_Church_ID=" . $_SESSION['iChurchID'] ;
			RunQuery($sSQL);

			// Update the donation amounts table.  Easiest to just delete old entries first..
			$sSQL = "DELETE FROM donationamounts_dna WHERE dna_don_ID = $iDonationID AND chu_Church_ID=" . $_SESSION['iChurchID'];
			RunQuery($sSQL);

			// Loop through, ignoring any rows with a missing fund or amount;
			for ($iRow=1; $iRow <= $iFundEntries; $iRow++)
			{
				if ($aFundIDs[$iRow] > 0 && $aFundAmounts[$iRow] > 0)
				{
					$sSQL = "INSERT INTO donationamounts_dna VALUES
							($iDonationID, " . $_SESSION['iChurchID'] . ",  $aFundAmounts[$iRow], $aFundIDs[$iRow])";
					RunQuery($sSQL);
				}
			}
		}
		// Adding new donation
		else
		{
			if ($iEnvelope <= 0) $iEnvelope = "NULL";

			$sSQL = "INSERT INTO donations_don VALUES
				('', " . $_SESSION['iChurchID'] . ",  $iPersonID, $iPaymentType, $iCheckNumber, '$sDate', $iEnvelope)";

			RunQuery($sSQL);

			$sSQL = "SELECT MAX(don_ID) AS iDonationID FROM donations_don WHERE chu_Church_ID=" . $_SESSION['iChurchID'];
			$rsLastEntry = RunQuery($sSQL);
			extract(mysql_fetch_array($rsLastEntry));

			for ($iRow=1; $iRow <= $iFundEntries; $iRow++)
			{
				if ($aFundIDs[$iRow] > 0 && $aFundAmounts[$iRow] > 0)
				{
					$sSQL = "INSERT INTO donationamounts_dna VALUES
							($iDonationID, " . $_SESSION['iChurchID'] . ",  $aFundAmounts[$iRow], $aFundIDs[$iRow])";
					RunQuery($sSQL);
				}
			}
		}

		// Route back to the donation list of this person or add another entry in batch mode
		if ($batchMode)
			Redirect("DonationEditor.php?Batch=1");
		else
			Redirect("DonationView.php?PersonID=$iPersonID");
	}
}
else if (isset($_POST["SearchDonor"]))
{
	$sLastOrEnv = FilterInput($_POST["DonorLast"]);

	if (is_numeric($sLastOrEnv))
	{
		$iEnvelopeID = $sLastOrEnv;
		$bWasEnvelopeID = true;
		$sSQL = "SELECT per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix, per_Envelope
				FROM person_per
				WHERE per_Envelope=" . $iEnvelopeID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
	}
	else
	{
		$sDonorLast = $sLastOrEnv;
		$bWasEnvelopeID = false;
		$sSQL = "SELECT per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix, per_Envelope
				FROM person_per
				WHERE per_LastName like '" . $sDonorLast . "%' AND per_Envelope IS NOT NULL AND chu_Church_ID=" . $_SESSION['iChurchID'];
	}

	$rsDonorEnvelopes = RunQuery($sSQL);
	$numDonors = mysql_num_rows($rsDonorEnvelopes);

	for ($iRow = 1; $iRow <= $numDonors; $iRow++)
	{
		$aRow = mysql_fetch_array($rsDonorEnvelopes);
		$aFullName[$iRow] = "";
		if ($aRow['per_Title'] != "") { $aFullName[$iRow] .= $aRow['per_Title'] . " "; }
		if ($aRow['per_FirstName'] != "") { $aFullName[$iRow] .= $aRow['per_FirstName'] . " "; }
		if ($aRow['per_MiddleName'] != "") { $aFullName[$iRow] .= $aRow['per_MiddleName'] . " "; }
		if ($aRow['per_LastName'] != "") { $aFullName[$iRow] .= $aRow['per_LastName'] . " "; }
		if ($aRow['per_Suffix'] != "") { $aFullName[$iRow] .= $aRow['per_Suffix'] . " "; }
		$aEnvelopeID[$iRow] = $aRow['per_Envelope'];
	}

	if ($numDonors > 0)
	{
		$iEnvelope = $aEnvelopeID[1];
	}
}
// First pass
else
{
	// If editing, get existing data for this donation
	if ($editorMode == 1)
	{
		$sSQL = "SELECT don_DonorID as iPersonID, don_CheckNumber as iCheckNumber,
						don_Date as sDate, don_PaymentType as iPaymentType, don_Envelope as iEnvelope
				FROM donations_don
				WHERE don_ID=" . $iDonationID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
		$rsDonation = RunQuery($sSQL);
		extract(mysql_fetch_array($rsDonation));

		$sSQL = "SELECT dna_Amount, dna_fun_ID FROM donationamounts_dna WHERE dna_don_ID = $iDonationID and chu_Church_ID=" . $_SESSION['iChurchID'] . " ORDER BY dna_Amount";
		$rsDonationAmounts = RunQuery($sSQL);
		$numDonAmtRows = mysql_num_rows($rsDonationAmounts);

		$totalAmount = 0;
		for($iRow = 1; $iRow <= $numDonAmtRows; $iRow++)
		{
			$aRow = mysql_fetch_array($rsDonationAmounts);
			$aFundIDs[$iRow] = $aRow['dna_fun_ID'];
			$aFundAmounts[$iRow] = $aRow['dna_Amount'];
			$totalAmount += $aFundAmounts[$iRow];
		}
	}

	// Otherwise, initialize defaults.
	else
	{
		$totalAmount = 0;
		if ($batchMode)
		{
			$iPaymentType = $_SESSION['iDefaultPaymentType'];
			$aFundIDs[1] = $_SESSION['iDefaultFundID'];
			$sDate = $_SESSION['sDefaultDate'];
		}
		else
		{
			$today = getdate();
			$sDate = $today['year'] . "-" . $today['mon'] . "-" . $today['mday'];
		}
	}
}

$sSQL = "SELECT fun_ID,fun_Name,fun_Description,fun_Active FROM donationfund_fun";
if ($editorMode == 0) $sSQL .= " WHERE fun_Active = 'true' AND chu_Church_ID=" . $_SESSION['iChurchID']; // New donations should show only active funds.
$rsFunds = RunQuery($sSQL);

if (!$batchMode)
{
	$sSQL = "SELECT per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix,
	per_Address1, per_Address2, per_City, per_State, per_Zip, per_Country,
	fam_Address1, fam_Address2, fam_City, fam_State, fam_Zip, fam_Country
	FROM person_per	LEFT JOIN family_fam ON person_per.per_fam_ID = family_fam.fam_ID
	WHERE per_ID=" . $iPersonID . " AND person_per.chu_Church_ID=" . $_SESSION['iChurchID'];
	$rsPerson = RunQuery($sSQL);
	extract(mysql_fetch_array($rsPerson));

	$sCity = SelectWhichInfo($per_City, $fam_City, false);
	$sState = SelectWhichInfo($per_State, $fam_State, false);
	$sZip = SelectWhichInfo($per_Zip, $fam_Zip, false);
	$sCountry = SelectWhichInfo($per_Country, $fam_Country, false);

	SelectWhichAddress($sAddress1, $sAddress2, $per_Address1, $per_Address2, $fam_Address1, $fam_Address2, false);
}

require "Include/Header.php";

?>
<script type="text/javascript">
<?php
if ($batchMode) { ?>
	var IFrameObj; // our IFrame object

	function UpdateAddress(person_ID)
	{
		//var person_ID = document.getElementById('EnvID').value;
		if (!document.createElement) {return true};
		var IFrameDoc;
		var URL = 'RPCdummy.php?mode=Envelope2Address&data=' + person_ID;
		if (!IFrameObj && document.createElement) {
			var tempIFrame=document.createElement('iframe');
			tempIFrame.setAttribute('id','RSIFrame');
			tempIFrame.style.border='0px';
			tempIFrame.style.width='0px';
			tempIFrame.style.height='0px';
			IFrameObj = document.body.appendChild(tempIFrame);

			if (document.frames) {
				// For IE5 Mac
				IFrameObj = document.frames['RSIFrame'];
			}
		}

		if (navigator.userAgent.indexOf('Gecko') !=-1
			&& !IFrameObj.contentDocument) {
			// For NS6
			setTimeout('AddToCart()',10);
			return false;
		}

		if (IFrameObj.contentDocument) {
			// For NS6
			IFrameDoc = IFrameObj.contentDocument;
		} else if (IFrameObj.contentWindow) {
			// For IE5.5 and IE6
			IFrameDoc = IFrameObj.contentWindow.document;
		} else if (IFrameObj.document) {
			// For IE5
			IFrameDoc = IFrameObj.document;
		} else {
			return true;
		}

		IFrameDoc.location.replace(URL);
		return false;
	}

	function updateAddressInfo(generated_html)
	{
		if (generated_html == "invalid") {
			document.getElementById('AddressInfo').innerHTML = '<p class="LargeError"><?php echo gettext("Invalid Envelope Number!") ?><p>';
			document.getElementById('EnvID').value = '';
		} else {
			document.getElementById('AddressInfo').innerHTML = generated_html;
		}
	}

	function UpdateInfo()
	{
		document.getElementById('Envelope').innerHTML = '<b><?php echo gettext("Envelope #:"); ?></b><br />' +
															document.forms['DonationForm'].DonorList.value;
		UpdateAddress(document.forms['DonationForm'].DonorList.value);
	}

<?php } ?>
	function clearForm(isBatchMode,iFundEntries,prevDate)
	{
		document.forms['DonationForm'].DonorLast.value = '';
		document.getElementById('DonorListDiv').innerHTML = '<b><?php echo gettext("Selected donor:"); ?></b>';
		document.getElementById('Envelope').innerHTML = '<b><?php echo gettext("Envelope #:"); ?></b>';
		document.forms['DonationForm'].CheckNumber.value = '';
		document.forms['DonationForm'].Date.value = prevDate;
		document.getElementById('total').innerHTML = '0';

		if (isBatchMode)
		{
			document.forms['DonationForm'].PaymentType.selectedIndex = document.forms['DonationForm'].DefaultPaymentType.selectedIndex;
			document.getElementById('AddressInfo').innerHTML = '<b><?php echo gettext("Address Info:"); ?></b>';
			document.forms['DonationForm'].Date.value = document.forms['DonationForm'].DefaultDate.value;
		}
		else
		{
			document.forms['DonationForm'].PaymentType.selectedIndex = '';
			document.forms['DonationForm'].Date.value = prevDate;
		}

		for (fundRow = 1; fundRow <= iFundEntries; fundRow++)
		{
			document.forms['DonationForm'].elements['FundID' + fundRow].selectedIndex = 0;
			document.forms['DonationForm'].elements['FundAmount' + fundRow].value = '';
		}

		if (isBatchMode)
			document.forms['DonationForm'].FundID1.selectedIndex = document.forms['DonationForm'].DefaultFundID.selectedIndex;
	}

	function updateTotal()
	{
		var total = 0;
		for (fundRow = 1; fundRow <= <?php echo $iFundEntries;?>; fundRow++)
		{
			var thisValue = parseFloat(document.forms['DonationForm'].elements['FundAmount' + fundRow].value);
			if (!isNaN(thisValue))
				total += thisValue;
		}
		document.getElementById('total').innerHTML = total;
	}

</script>

<?php
echo "<form name=\"DonationForm\" method=\"post\" action=\"" . $_SERVER['PHP_SELF'] . "?PersonID=" . $iPersonID . "&DonationID=" . $iDonationID;
if ($batchMode) echo "&Batch=1";
echo "\">";
?>
<input type="hidden" value="<?php echo $iPersonID;?>" name="DonorID">

<?php if ($batchMode) { ?>
<p class="LargeText" align="center"><?php echo gettext("Batch Entry Mode"); ?></p>
<table align="center" cellpadding="3" border=1>
	<tr>
		<td class="TextColumn">
			<b><?php echo gettext("Default Date:"); ?></b><br>
			<input type="text" Name="DefaultDate" id="DefaultDateField" value="<?php echo $_SESSION['sDefaultDate']; ?>" size="15" maxlength="15" onfocus="ClearFieldOnce(this);"><input type="image" onclick="return showCalendar('DefaultDateField', 'y-mm-dd');" src="Images/calendar.gif">
		</td>
		<td class="TextColumn">
			<b><?php echo gettext("Default fund:"); ?></b><br>
			<select name="DefaultFundID">
			<option value="0"><?php echo gettext("None"); ?></option>
			<?php
				while ($row = mysql_fetch_array($rsFunds))
				{
					$fun_id = $row["fun_ID"];
					$fun_name = $row["fun_Name"];
					echo "<option value=\"$fun_id\" " ;
					if ($_SESSION['iDefaultFundID']==$fun_id)
						echo "selected" ;
					echo ">$fun_name</option>" ;
				}
			?>
			</select>
		</td>
		<td class="TextColumn">
			<b><?php echo gettext("Default Type:"); ?></b><br>
			<select name="DefaultPaymentType">
			<option <?php if ($_SESSION['iDefaultPaymentType']==1) echo "selected " ?>value="1"><?php echo gettext("cash"); ?></option>
			<option <?php if ($_SESSION['iDefaultPaymentType']==2) echo "selected " ?>value="2"><?php echo gettext("check"); ?></option>
			<option <?php if ($_SESSION['iDefaultPaymentType']==3) echo "selected " ?>value="3"><?php echo gettext("credit"); ?></option>
			</select>
     	</td>
	</tr>
</table>
<?php } ?>

<table align="center" cellspacing="5" cellpadding="5" border=1 width="40%">
	<tr>
		<td width="10%">
			<?php
			if ($editorMode == 0)
			{
				if ($batchMode)
					$batchOption = 'true';
				else
					$batchOption = 'false';
				echo "<br><input class=\"icButton\" type=\"button\" value=\"" . gettext("Clear") . "\" onclick=\"clearForm($batchOption,$iFundEntries,'$sDate');\">";
			}
			else
			{ ?>
				<p align="center"><b><?php echo gettext("Receipt Number:"); ?></p>
				<p align="center" class="LargeText"><?php echo $iDonationID; ?></p></b><br>
				<input type="button" <?php echo 'value="' . gettext("delete") . '"'; ?> Name="delete" onclick="window.location='DonationDelete.php?donID=<?php echo $iDonationID; ?>';">
			<?php } ?>
		</td>
		<td width="90%">
			<table align="center" cellpadding="10">
				<tr>
					<td>
						<b><?php echo gettext("Donation Date:"); ?></b><br>
						<input type="text" Name="Date" id="DateField" value="<?php echo $sDate; ?>" size="15" maxlength="15" onfocus="ClearFieldOnce(this);"><input type="image" onclick="return showCalendar('DateField', 'y-mm-dd');" src="Images/calendar.gif">
						<font color="red"><?php echo $sDateError; ?></font>
					</td>
				</tr>
				<tr>
					<td>
						<b><?php echo gettext("Envelope # or donor's last name:"); ?></b><br />
						<input type="text" Name="DonorLast" id="Donor" value="<?php echo $sLastOrEnv; ?>" size="25" maxlength="50" />
					</td>
					<td>
						<br />
						<input type="submit" class="icButton" value="<?php echo gettext("Search"); ?>" Name="SearchDonor">
					</td>
				</tr>
				<tr>
					<td>
						<div id="DonorListDiv" class="ShadedBox">
						<b><?php echo gettext("Selected donor:") ?></b>
				
<?php
if (isset($numDonors))
{
	if ($numDonors > 0)
	{
		$iSize = $numDonors;
		if ($iSize > 5)
		{
			$iSize = 5;
		}
?>
						<select id="DonorList" name="DonorList" size="<?php echo $iSize ?>" onChange="UpdateInfo();">
							<option selected value="<?php echo $aEnvelopeID[1] ?>"><?php echo $aFullName[1] ?></option>
<?php
		for($iRow = 2; $iRow <= $numDonors; $iRow++)
		{
?>
							<option value="<?php echo $aEnvelopeID[$iRow] ?>"><?php echo $aFullName[$iRow] ?></option>
<?php
		}
?>
						</select>
<?php
	}
	else
	{
?>
				
						<br />
						<font color="red">
<?php
		if ($bWasEnvelopeID == false)
		{
			echo gettext("No person with this last name and an assigned envelope # was found!");
		}
?>
						</font>
<?php
	}
}
?>
						</div>
					</td>
				</tr>
				<tr>
					<td nowrap>
						<div id="Envelope" class="ShadedBox">
						<b><?php echo gettext("Envelope #:"); ?></b><br />
						<font color="red"><?php echo $sEnvelopeError; ?></font>
<?php
		if (isset($bWasEnvelopeID) && ($bWasEnvelopeID == true) && ($numDonors == 0))
		{
			echo "<font color=\"red\">";
			echo gettext("Invalid envelope number!");
			echo "</font>";
		}
		else
		{
			echo $iEnvelope;
		}
?>
						</div>
						<?php if (isset($iEnvelope) && ($iEnvelope != "")) echo "<script type=\"text/javascript\">UpdateAddress(" . $iEnvelope . ");</script>" ?>
					</td>
					<td nowrap>
						<b><?php echo gettext("Payment Type:"); ?></b><br>
						<select Name="PaymentType" id="PaymentType">
						<option <?php if ($iPaymentType==1) echo "selected " ?>value="1"><?php echo gettext("cash"); ?></option>
						<option <?php if ($iPaymentType==2) echo "selected " ?>value="2"><?php echo gettext("check"); ?></option>
						<option <?php if ($iPaymentType==3) echo "selected " ?>value="3"><?php echo gettext("credit"); ?></option>
						</select>
						<br><font color="red"><?php echo $sPaymentError; ?></font>
					</td>
					<td nowrap>
						<b><?php echo gettext("Check Number:"); ?></b><br>
						<input type="text" Name="CheckNumber" id="CheckNumber" value="<?php echo $iCheckNumber; ?>" size="10" maxlength="10">
						<br><font color="red"><?php echo $sCheckError; ?></font>
					</td>
				</tr>
				<tr>
					<td colspan="2">
						<div id="AddressInfo" class="ShadedBox">
						<b><?php echo gettext("Address Info:"); ?></b><br>
						<?php
						if ($sAddress1 != "") { echo $sAddress1 . "<br>"; }
						if ($sAddress2 != "") { echo $sAddress2 . "<br>"; }
						if ($sCity != "") { echo $sCity . ", "; }
						if ($sState != "") { echo $sState; }
						if ($sZip != "") { echo " " . $sZip; }
						if ($sCountry != "") {echo "<br>" . $sCountry; }
						?>
						</div>
					</td>
				</tr>
			</table>
		</td>
	</tr>

	<tr>
		<td>&nbsp;</td>
		<td>
			<?php if ($noValid) echo "<font color=\"red\">" . gettext("You must enter at least one valid fund / amount row.") . "</font>";?>
			<table align="center" width="80%">
				<tr>
					<th class="HeaderRow" width="20%">&nbsp;</th>
					<th class="HeaderRow" width="60%"><?php echo gettext("Fund"); ?></th>
					<th class="HeaderRow" width="20%"><?php echo gettext("Amount"); ?></th>
				</tr>
			<?php
			for ($fundRow = 1; $fundRow <= $iFundEntries; $fundRow++)
			{ ?>
				<tr>
					<td class="LabelColumn"><?php echo $fundRow; ?></td>
					<td class="TextColumn" align="center">
						<select name="FundID<?php echo $fundRow ?>">
						<option value="0"><?php echo gettext("None"); ?></option>
						<?php
						mysql_data_seek($rsFunds,0);
						while ($row = mysql_fetch_array($rsFunds))
						{
							$fun_id = $row["fun_ID"];
							$fun_name = $row["fun_Name"];
							$fun_active = $row["fun_Active"];
							echo "<option value=\"$fun_id\" " ;
							if ($aFundIDs[$fundRow] == $fun_id)
								echo "selected" ;
							echo ">$fun_name";
							if ($fun_active != 'true') echo " (" . gettext("inactive") . ")";
							echo "</option>" ;
						}
						?>
						</select>
					</td>
					<td class="TextColumn" align="center">
						<input name="FundAmount<?php echo $fundRow ?>" type="text" value="<?php echo $aFundAmounts[$fundRow]; ?>" size="7" onChange="updateTotal();">
					</td>
				</tr>
			<?php } ?>
				<tr>
					<td>&nbsp;</td>
					<td class="MediumText" align="right"><b><?php echo gettext("Total:"); ?></b></td>
					<td class="MediumText" align="center"><b><div id="total"><?php echo "\$$totalAmount";?></div></b></td>
				</tr>
			</table>
		</td>
	</tr>
</table>
<br>
<p align="center">
	<input type="submit" class="icButton" <?php echo 'value="' . gettext("Enter Donation") . '"'; ?> Name="DonationSubmit">
	&nbsp;
	<input type="button" class="icButton" <?php echo 'value="' . gettext("Exit") . '"'; ?> Name="DonationCancel"
	<?php
	if (!$batchMode)
		echo "onclick=\"javascript:document.location='DonationView.php?PersonID=" . $iPersonID . "';\">";
	else
		echo "onclick=\"javascript:document.location='Menu.php'\">";
	?>
</p>

</form>

<?php
	require "Include/Footer.php";
?>
