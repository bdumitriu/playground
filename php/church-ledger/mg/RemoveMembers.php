<?php

/**
 * This script handles requests to remove members from a management group.
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

// collect the members we need to remove in the $members array
foreach ($_POST as $key => $value)
{
	if ($value == "on")
	{
		$members[] = new Member($key);
	}
}

if (is_null($members))
{
	echo "<h3>You have selected no members to remove.</h3>\n";
}
else if ($mg->loadMembersFromDB())
{
	$mg->removeMembers($members);
	if ($mg->writeMembersToDB())
	{
		echo "<h3>All members removed successfully.</h3>\n";
	}
	else
	{
		echo "<h3>Error while removing members from membership group.</h3>\n";
		echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
	}
}
else
{
	echo "<h3>Error in retrieving membership group data from the database.\n";
	echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
}
?>

<br /><br />
Back to <a href="ListMG.php">listing</a> of membership groups.

</body>

</html>