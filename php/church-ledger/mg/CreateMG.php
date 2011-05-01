<?php

/**
 * This script handles requests to create a new membership group.
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
if ($mg->create())
{
	echo "<h3>Membership group \"" . $_POST["name"] . "\" created successfully.</h3>\n\n";
}
else
{
	echo "<h3>Error in creating membership group.</h3>\n";
	echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
}

?>

<br /><br />
Back to membership group creation <a href="CreateMG.html">page</a>.

</body>

</html>
