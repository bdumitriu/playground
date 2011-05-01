<?php
 /* 
 *	Date: 2004-11-28
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
*/
require "Include/Config.php";
require "Include/Functions.php";

$mode = $_GET['mode'];
$data = FilterInput($_GET['data'],'int');
?>

<html>
<head>
<script language="JavaScript">
<?php
// Select the appropriate Javascript routine..
switch ($mode)
{
	case CartCounter:
		?>
			windowOnload = function()
			{
				window.parent.updateCartCounter('<?php echo count($_SESSION['aPeopleCart']); ?>');
			}
		<?php
	break;

	case Envelope2Address:
		// Security check
		if (!$_SESSION['bFinance']) exit;

		$sSQL = "SELECT per_Address1, per_Address2, per_City, per_State, per_Zip, per_Country,
						fam_Address1, fam_Address2, fam_City, fam_State, fam_Zip, fam_Country
					FROM person_per	LEFT JOIN family_fam ON person_per.per_fam_ID = family_fam.fam_ID
					WHERE per_Envelope=" . $data;
		$rsQuery = RunQuery($sSQL);

		if (mysql_num_rows($rsQuery) == 0)
		{
			$sAddressInfoHTML = "invalid";
		}
		else
		{
			extract(mysql_fetch_array($rsQuery));
			
			$sCity = SelectWhichInfo($per_City, $fam_City, false);
			$sState = SelectWhichInfo($per_State, $fam_State, false);
			$sZip = SelectWhichInfo($per_Zip, $fam_Zip, false);
			$sCountry = SelectWhichInfo($per_Country, $fam_Country, false);

			SelectWhichAddress($sAddress1, $sAddress2, $per_Address1, $per_Address2, $fam_Address1, $fam_Address2, false);

			$sAddressInfoHTML = "<b>" . gettext("Address Info:") . "</b><br>";
			if ($sAddress1 != "") { $sAddressInfoHTML .= $sAddress1 . "<br>"; }
			if ($sAddress2 != "") { $sAddressInfoHTML .= $sAddress2 . "<br>"; }
			if ($sCity != "") { $sAddressInfoHTML .= $sCity . ", "; }
			if ($sState != "") { $sAddressInfoHTML .= $sState; }
			if ($sZip != "") { $sAddressInfoHTML .= " " . $sZip; }
			if ($sCountry != "") {$sAddressInfoHTML .= "<br>" . $sCountry; }
		}
		?>
			windowOnload = function()
			{
				window.parent.updateAddressInfo('<?php echo $sAddressInfoHTML; ?>');
			}
		<?php
	break;

	case GroupRolesSelect:
		// Security check
		if (!$_SESSION['bManageGroups']) exit;

		if ($data > 0) {
			$sSQL = "SELECT grp_DefaultRole,grp_RoleListID FROM group_grp WHERE grp_ID = " . $data;
			$rsQuery = RunQuery($sSQL);
			extract(mysql_fetch_array($rsQuery));

			$sSQL = "SELECT lst_OptionID,lst_OptionName FROM list_lst WHERE lst_ID = " . $grp_RoleListID . " ORDER BY lst_OptionSequence";
			$rsQuery = RunQuery($sSQL);

			if (mysql_num_rows($rsQuery) == 0)
				$sGeneratedHTML = "invalid";
			else
			{
				$sGeneratedHTML = "<select name=\"GroupRole\">";
				while($aRow = mysql_fetch_array($rsQuery))
				{
					extract($aRow);
					$sGeneratedHTML .= "<option value=\"" . $lst_OptionID . "\"";
					if ($lst_OptionID == $grp_DefaultRole) $sGeneratedHTML .= " selected";
					$sGeneratedHTML .= ">" . $lst_OptionName . "</option>";
				}
				$sGeneratedHTML .= "</select>";
			}
		} else {
			$sGeneratedHTML = gettext("No Group Selected");
		}
		?>
			windowOnload = function()
			{
				window.parent.updateGroupRoles('<?php echo $sGeneratedHTML; ?>');
			}
		<?php
	break;
}
?>
</script>
</head>
<body onload="windowOnload();">
</body>
</html>
