<?php

//Include the function library
require "Include/Config.php";
require "Include/Functions.php";
require "Include/ReportFunctions.php";

//Get the GroupID out of the querystring
$iGroupID = FilterInput($_GET["GroupID"],'int');

$bError = True;

// Was the form submitted?
if (isset($_POST["submit"]))
{
	// Assign all the stuff locally
	$sFrom = stripslashes($_POST["from"]);
	$sEmailTitle = stripslashes($_POST["emailtitle"]);
	$sEmailMessage = stripslashes($_POST["emailmessage"]);

	$bError = False;

	if ($sFrom == "")
	{
		$sFromError = "<br><font color=\"red\">" . gettext("From field cannot be empty.") . "</font>";
		$bError = True;
	}

	if ($sEmailTitle == "")
	{
		$sEmailTitleError = "<br><font color=\"red\">" . gettext("Subject field cannot be empty.") . "</font>";
		$bError = True;
	}

	if ($sEmailMessage == "")
	{
		$sEmailMessageError = "<br><font color=\"red\">" . gettext("Message cannot be empty.") . "</font>";
		$bError = True;
	}
}

if ($bError)
{
	//Set the page title
	$sPageTitle = gettext("Send an Email to the Group");


	//Get all the members of this group
	$sSQL = "SELECT per_Email  FROM person_per, person2group2role_p2g2r WHERE per_ID = p2g2r_per_ID AND p2g2r_grp_ID = " . $iGroupID . " AND person_per.chu_Church_ID=" . $_SESSION['iChurchID'];
	$rsGroupMembers = RunQuery($sSQL);

	//Collect all the email addresses in a variable
	$toField = "";
	$first = true;

	require "Include/Header.php";
?>

<form name="groupEmailForm" method="post" action="<?php echo $_SERVER['PHP_SELF'] . "?GroupID=" . $iGroupID; ?>">
<!-- <form name="groupEmailForm" method="post" action="EmailSend.php" method="POST"> -->

<?php

	//Loop through the recordset
	while ($aRow = mysql_fetch_array($rsGroupMembers))
	{
		extract($aRow);

		//Add each person to the cart
		if ($per_Email != "")
		{
			echo "<input type=\"hidden\" name=\"emaillist[]\" value=\"" . $per_Email . "\">\n";
			if ($first)
				$first = false;
			else
				$toField .= ", ";
			$toField .= $per_Email;
		}
	}

?>

<table cellpadding="4">
	<tr>
		<td class="LabelColumn"><b><?php echo gettext("To:"); ?></b></td>
		<td class="TextColumn"><?php echo $toField; ?></td>
	</tr>
	<tr>
		<td class="LabelColumn"><b><?php echo gettext("From:"); ?></b></td>
		<td class="TextColumn">
			<input type="text" name="from" value="<?php echo $sFrom; ?>" size="50">
			<?php echo $sFromError; ?>
		</td>
	</tr>
	<tr>
		<td class="LabelColumn"><b><?php echo gettext("Subject:"); ?></b></td>
		<td class="TextColumn">
			<input type="text" name="emailtitle" value="<?php echo $sEmailTitle; ?>" size="50">
			<?php echo $sEmailTitleError; ?>
		</td>
	</tr>
	<tr>
		<td class="LabelColumn"><b><?php echo gettext("Message:"); ?></b></td>
		<td class="TextColumn">
			<textarea name="emailmessage" rows="20" cols="72"><?php echo $sEmailMessage; ?></textarea>
			<?php echo $sEmailMessageError; ?>
		</td>
	</tr>
	<tr>
		<td colspan="2" align="center"><input class="icButton" type="submit" name="submit" value="<?php echo gettext("Send Email"); ?>"></td>
	</tr>
</table>

</form>

<script language="JavaScript"><!--
document.groupEmailForm.emailtitle.focus();
//--></script>

<?php

}
else
{
	// Load the PHPMailer library
	LoadLib_PHPMailer();

	//Set the page title
	$sPageTitle = gettext("Email Sent");

	require "Include/Header.php";

	$mail = new ICMail;

	$email_array = $_POST['emaillist'];

	if ( is_array($email_array) == TRUE )
	{
		$mail->IsSMTP();
		$mail->SMTPKeepAlive = true;
		$mail->FromName = stripslashes($_POST['from']);
		$mail->From = stripslashes($_POST['from']);
		$mail->Subject = stripslashes($_POST['emailtitle']);
		$mail->Body = stripslashes($_POST['emailmessage']);
		foreach ($email_array as $email_address)
		{

			$mail->AddAddress($email_address);
			echo '<b>' . $email_address . '</b><br />';
			if (!$mail->Send())
				echo "There has been a mail error sending to " . $email_address . "<br>";
			$mail->ClearAddresses();
		}
		$mail->SmtpClose();
	}
	else
	{
		echo gettext("No email addresses specified!");
	}
}

require "Include/Footer.php";

?>
