<?php

/**
 * This script handles requests to add members in a management group.
 */

require_once "ConnectionData.php";
require_once "MembershipGroup.php";
require_once "Member.php";

$mg = new MembershipGroup();

?>

<html>

<head>
<title>Church Ledger - Membership Group Management Module</title>
</head>

<body>

<?php

$mg->setId($_POST["id"]);

// collect the members we need to add in the $members array
foreach ($_POST as $key => $value)
{
	if ($value == "on")
	{
		$members[] = new Member($key);
	}
}

$mg->addMembers($members);

if (is_null($members))
{
	echo "<h3>You have selected no members to add.</h3>\n";
}
else if ($mg->appendMembersToDB())
{
	echo "<h3>All members added successfully.</h3>\n";
}
else
{
	echo "<h3>Error while adding members to membership group.</h3>\n";
	echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
}
?>

<br /><br />
Back to <a href="ListMG.php">listing</a> of membership groups.

</body>

</html>