<?php
/*******************************************************************************
 *
 *  filename    : ReportList.php
 *  last change : 2003-03-20
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
 ******************************************************************************/

require "Include/Config.php";
require "Include/Functions.php";

//Set the page title
$sPageTitle = gettext("Report Menu");

$today = getdate();
$year = $today['year'];

require "Include/Header.php";

// Finance reports are only visible to those with this permission
if ($_SESSION['bFinance']) {
?>
	<p>
	<a class="MediumText" href="DonationReports.php"><?php echo gettext("Donation Summary Report"); ?></a>
	<br>
	<?php echo gettext("Displays a donation summary report for a specified day, including breakdown by fund."); ?>
	</p>

	<p>
	<a class="MediumText" href="Reports/DonationReportYearly.php?year=<?php echo $year; ?>"><?php echo gettext("Donation End of Year Reports for All Members") . " - " . $year; ?></a>
	&nbsp;/&nbsp;
	<a class="MediumText" href="Reports/DonationReportYearly.php?year=<?php echo ($year - 1); ?>"><?php echo gettext("OR") . " " . ($year - 1); ?></a>
	<br>
	<?php echo gettext("Member Contribution Letter for all members with donations. (multi-page PDF)"); ?>
	</p>

	<p>
	<a class="MediumText" href="Reports/TitheReport.php?year=<?php echo $year; ?>"><?php echo gettext("Tithe Reports for All Members") . " - " . $year; ?></a>
	&nbsp;/&nbsp;
	<a class="MediumText" href="Reports/TitheReport.php?year=<?php echo ($year - 1); ?>"><?php echo gettext("OR") . " " . ($year - 1); ?></a>
	<br>
	<?php echo gettext("Tithe reports for all members who have a tithe set for this year. (multi-page PDF)"); ?>
	</p>
<?php } ?>

<p>
<a class="MediumText" href="GroupReports.php"><?php echo gettext("Reports on groups and roles"); ?></a>
<br>
<?php echo gettext("Report on group and roles selected (it may be a multi-page PDF)."); ?>
</p>


<?php if ($_SESSION['bAdmin'] || !$bCSVAdminOnly) { ?>
	<p>
	<a class="MediumText" href="DirectoryReports.php"><?php echo gettext("Members Directory"); ?></a>
	<br>
	<?php echo gettext("Printable directory of all members, grouped by family where assigned"); ?>
	</p>
<?php } ?>

<?php /*
<p>
<a href=""><?php echo gettext("Members Directory w/Photos"); ?></a>
<br>
<?php echo gettext("Printable directory of all members. Family photos where available / Individual photos otherwise."); ?>
</p> */ ?>

<?php
require "Include/Footer.php";
?>
