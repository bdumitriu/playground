<?php

require "Include/Config.php";
require "Include/Functions.php";

if (!$_SESSION['bFinance'])
{
	Redirect("Menu.php");
	exit;
}

$sPageTitle = gettext("Tithe Entry");
require "Include/Header.php";

if (isset($_POST["SetTithe"]))
{
	$iPID = FilterInput($_POST["PersonList"], 'int');
	$iYear = FilterInput($_POST["TitheYear"], 'int');
	$fAmount = FilterInput($_POST["TitheValue"], 'float');
	$iChurchID = $_SESSION['iChurchID'];
	$iEditedBy = $_SESSION['iUserID'];

	$bError = false;
	if (!is_int($iPID))
	{
		$sNameError = gettext("You must select a person.");
		$bError = true;
	}

	if (!is_int($iYear))
	{
		$sYearError = gettext("You must enter a year.");
		$bError = true;
	}
	else if ($iYear == 0)
	{
		$sYearError = gettext("You must enter a valid year.");
		$bError = true;
	}

	if (!is_float($fAmount))
	{
		$sTitheError = gettext("You must enter an amount for the tithe.");
		$bError = true;
	}
	else if ($fAmount == 0)
	{
		$sTitheError = gettext("You must enter a valid amount for the tithe.");
		$bError = true;
	}

	if ($bError)
	{
		$sTitheYear = $_POST["TitheYear"];
		$sTitheValue = $_POST["TitheValue"];
	}

	if ($bError && is_int($iPID) && $_POST["LastName"] != "")
	{
		$sLastName = FilterInput($_POST["LastName"]);
		$sSQL = "SELECT per_ID, per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix
				FROM person_per
				WHERE per_LastName like '" . $sLastName . "%' AND chu_Church_ID=" . $_SESSION['iChurchID'];
		$iSelID = $iPID;
		$rsPers = RunQuery($sSQL);
		$numPers = mysql_num_rows($rsPers);
	
		for ($iRow = 1; $iRow <= $numPers; $iRow++)
		{
			$aRow = mysql_fetch_array($rsPers);
			$aFullName[$iRow] = "";
			if ($aRow['per_Title'] != "") { $aFullName[$iRow] .= $aRow['per_Title'] . " "; }
			if ($aRow['per_FirstName'] != "") { $aFullName[$iRow] .= $aRow['per_FirstName'] . " "; }
			if ($aRow['per_MiddleName'] != "") { $aFullName[$iRow] .= $aRow['per_MiddleName'] . " "; }
			if ($aRow['per_LastName'] != "") { $aFullName[$iRow] .= $aRow['per_LastName'] . " "; }
			if ($aRow['per_Suffix'] != "") { $aFullName[$iRow] .= $aRow['per_Suffix'] . " "; }
			$aPersonID[$iRow] = $aRow['per_ID'];
			if ($iPID == $aPersonID[$iRow])
			{
				$notSetName = $aFullName[$iRow];
			}
		}
		$sLastName = stripslashes($_POST["LastName"]);
	}
	else if (!$bError)
	{
		$sSQL = "SELECT per_person_ID
				FROM tithe_tth
				WHERE per_person_ID=" . $iPID . " AND tth_Year=" . $iYear . " AND chu_Church_ID=" . $iChurchID;
	
		$bExists = mysql_num_rows(RunQuery($sSQL)) > 0;
	
		if ($bExists == true)
		{
			$sSQL = "UPDATE tithe_tth
					SET tth_Amount=" . $fAmount . ", tth_LastEdited=now(), tth_LastEditedBy=" . $iEditedBy .
					" WHERE per_person_ID=" . $iPID . " AND tth_Year=" . $iYear . " AND chu_Church_ID=" . $iChurchID;
			RunQuery($sSQL);
			$sMessage = gettext("Tithe updated successfully.");
		}
		else
		{
			$sSQL = "INSERT INTO tithe_tth
					VALUES (DEFAULT, " . $iPID . ", " . $iChurchID . ", " . $fAmount . ", " . $iYear . ", now(), now(), " . $iEditedBy . ");";
			RunQuery($sSQL);
			$sMessage = gettext("Tithe set successfully.");
		}
	}
}
else if (isset($_POST["CheckTithe"]) || isset($_GET["CheckTithe"]))
{
	$sLastName = FilterInput($_POST["LastName"]);
	$iPID = FilterInput($_POST["PersonList"], 'int');
	$iYear = FilterInput($_POST["TitheYear"], 'int');
	$iChurchID = $_SESSION['iChurchID'];

	if (isset($_GET["CheckTithe"]))
	{
		$iPID = FilterInput($_GET["PersonList"], 'int');
		$iYear = FilterInput($_GET["TitheYear"], 'int');
	}
	
	$bError = false;
	if (!is_int($iPID))
	{
		$sNameError = gettext("You must select a person.");
		$bError = true;
	}

	if (!is_int($iYear))
	{
		$sYearError = gettext("You must enter a year.");
		$bError = true;
	}
	else if ($iYear == 0)
	{
		$sYearError = gettext("You must enter a valid year.");
		$bError = true;
	}

	if (is_int($iPID))
	{
		if ($sLastName == "")
		{
			$sSQL = "SELECT per_ID, per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix
					FROM person_per
					WHERE per_ID=" . $iPID . " AND chu_Church_ID=" . $_SESSION['iChurchID'];
		}
		else
		{
			$sSQL = "SELECT per_ID, per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix
					FROM person_per
					WHERE (per_LastName like '" . $sLastName . "%' OR per_ID=" . $iPID . ") AND chu_Church_ID=" . $_SESSION['iChurchID'];
		}
	
		$iSelID = $iPID;
	
		$rsPers = RunQuery($sSQL);
		$numPers = mysql_num_rows($rsPers);
	
		for ($iRow = 1; $iRow <= $numPers; $iRow++)
		{
			$aRow = mysql_fetch_array($rsPers);
			$aFullName[$iRow] = "";
			if ($aRow['per_Title'] != "") { $aFullName[$iRow] .= $aRow['per_Title'] . " "; }
			if ($aRow['per_FirstName'] != "") { $aFullName[$iRow] .= $aRow['per_FirstName'] . " "; }
			if ($aRow['per_MiddleName'] != "") { $aFullName[$iRow] .= $aRow['per_MiddleName'] . " "; }
			if ($aRow['per_LastName'] != "") { $aFullName[$iRow] .= $aRow['per_LastName'] . " "; }
			if ($aRow['per_Suffix'] != "") { $aFullName[$iRow] .= $aRow['per_Suffix'] . " "; }
			$aPersonID[$iRow] = $aRow['per_ID'];
			if ($iPID == $aPersonID[$iRow])
			{
				$notSetName = $aFullName[$iRow];
			}
		}

		if (!$bError)
		{
			$sSQL = "SELECT tth_Amount
					FROM tithe_tth
					WHERE per_person_ID=" . $iPID . " AND tth_Year=" . $iYear . " AND chu_Church_ID=" . $iChurchID;
		
			$rsTithe = RunQuery($sSQL);
			$bExists = mysql_num_rows($rsTithe) > 0;
		
			if ($bExists)
			{
				$sTitheYear = $iYear;
				$row = mysql_fetch_array($rsTithe);
				$sTitheValue = $row['tth_Amount'];
			}
			else
			{
				$sMessage = gettext("Tithe not set for ") . $notSetName . gettext(" in ") . $iYear . ".";
				$sTitheYear = stripslashes($_POST["TitheYear"]);
			}
		}
		else
		{
			$sTitheYear = stripslashes($_POST["TitheYear"]);
		}
	}
	else
	{
		$sTitheYear = stripslashes($_POST["TitheYear"]);
	}
}
else if (isset($_POST["SearchLastName"]))
{
	$sLastName = FilterInput($_POST["LastName"]);

	$sSQL = "SELECT per_ID, per_Title, per_FirstName, per_MiddleName, per_LastName, per_Suffix
			FROM person_per
			WHERE per_LastName like '" . $sLastName . "%' AND chu_Church_ID=" . $_SESSION['iChurchID'];

	$rsPers = RunQuery($sSQL);
	$numPers = mysql_num_rows($rsPers);

	for ($iRow = 1; $iRow <= $numPers; $iRow++)
	{
		$aRow = mysql_fetch_array($rsPers);
		$aFullName[$iRow] = "";
		if ($aRow['per_Title'] != "") { $aFullName[$iRow] .= $aRow['per_Title'] . " "; }
		if ($aRow['per_FirstName'] != "") { $aFullName[$iRow] .= $aRow['per_FirstName'] . " "; }
		if ($aRow['per_MiddleName'] != "") { $aFullName[$iRow] .= $aRow['per_MiddleName'] . " "; }
		if ($aRow['per_LastName'] != "") { $aFullName[$iRow] .= $aRow['per_LastName'] . " "; }
		if ($aRow['per_Suffix'] != "") { $aFullName[$iRow] .= $aRow['per_Suffix'] . " "; }
		$aPersonID[$iRow] = $aRow['per_ID'];
	}

	if ($numPers > 0)
	{
		$iSelID = $aPersonID[1];
	}

	$sLastName = stripslashes($_POST["LastName"]);
	$sTitheYear = stripslashes($_POST["TitheYear"]);
	$sTitheValue = stripslashes($_POST["TitheValue"]);
}

?>

<form name="TitheEntry" method="post" action="/osc/TitheEntry.php">

<?php
if (isset($sMessage))
{
?>
<div align="center"><b class="MediumLargeText"><?php echo $sMessage; ?></b></div>
<?php
}
?>

<table align="center" cellpadding="10">
	<tr>
		<td class="LabelColumn">
			<?php echo gettext("Last Name:") ?>
		</td>
		<td class="TextColumn">
			<input type="text" Name="LastName" id="LastName" value="<?php echo $sLastName; ?>" size="25" maxlength="50" />
		</td>
		<td>
			<input type="submit" class="icButton" value="<?php echo gettext("Search"); ?>" Name="SearchLastName">
		</td>
	</tr>
	<tr>
		<td />
		<td>
			<div id="PersonListDiv" class="ShadedBox">
			<b><?php echo gettext("Selected person:") ?></b>
			<br />
<?php
if (isset($numPers))
{
	if ($numPers > 0)
	{
		$iSize = $numPers;
		if ($iSize > 5)
		{
			$iSize = 5;
		}
?>
			<select id="PersonList" name="PersonList" size="<?php echo $iSize ?>">
<?php
		for($iRow = 1; $iRow <= $numPers; $iRow++)
		{
			if ($aPersonID[$iRow] == $iSelID)
			{
?>
				<option selected value="<?php echo $aPersonID[$iRow] ?>"><?php echo $aFullName[$iRow] ?></option>
<?php
			}
			else
			{
?>
				<option value="<?php echo $aPersonID[$iRow] ?>"><?php echo $aFullName[$iRow] ?></option>
<?php
			}
		}
?>
			</select>
<?php
	}
	else
	{
?>
			<font color="red">
			<?php echo gettext("No person with this last name was found!"); ?>
			</font>
<?php
	}
}
?>
			<?php if (isset($sNameError)) echo "<font color=\"red\">" . $sNameError . "</font>"; ?>
			</div>
		</td>
	</tr>
	<tr>
		<td class="LabelColumn">
			<?php echo gettext("Tithe for Year:") ?>
		</td>
		<td  class="TextColumn">
			<input type="text" Name="TitheYear" id="TitheYear" value="<?php echo $sTitheYear; ?>" size="7" />
			<?php if (isset($sYearError)) echo "<br><font color=\"red\">" . $sYearError . "</font>"; ?>
		</td>
	</tr>
	<tr>
		<td class="LabelColumn">
			<?php echo gettext("Tithe Value:") ?>
		</td>
		<td  class="TextColumn">
			<input type="text" Name="TitheValue" id="TitheValue" value="<?php echo $sTitheValue; ?>" size="7" />
			<?php if (isset($sTitheError)) echo "<br><font color=\"red\">" . $sTitheError . "</font>"; ?>
		</td>
	</tr>
	<tr>
		<td colspan="2" align="center">
			<input type="submit" class="icButton" value="<?php echo gettext("Check Tithe") ?>" name="CheckTithe">
			<input type="submit" class="icButton" value="<?php echo gettext("Set Tithe") ?>" name="SetTithe">
		</td>
	</tr>
</table>

</form>

<?php
	require "Include/Footer.php";
?>
