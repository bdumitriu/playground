<?php

/**
 * This script handles requests to update the information about a membership group.
 */

require_once "ConnectionData.php";
require_once "MembershipGroup.php";

$mg = new MembershipGroup();

?>

<html>

<head>
<title>Church Ledger - Membership Group Management Module</title>
</head>

<body>

<?php

$mg->loadFrom($_POST);
if ($mg->update())
{
	if ($mg->getLastRowCount() == 1)
	{
		echo "<h3>Membership group \"" . $_POST["name"] . "\" updated successfully.</h3>\n\n";
	}
	else
	{
		echo "<h3>Database contains no membership group with id " . $_POST["id"] . ".</h3>\n";
	}
}
else
{
	echo "<h3>Error in updating membership group.</h3>\n";
	echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
}

?>

<br /><br />
Back to <a href="ListMG.php">listing</a> of membership groups.

</body>

</html>