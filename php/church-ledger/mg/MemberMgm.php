<?php

/**
 * This script handles requests to manage the members in a management group.
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

<h3>Manage group members</h3>

<?php

$mg->setId($_GET["id"]);;

$noError = true;
$noError = $noError && $mg->loadFromDB();
$noError = $noError && $mg->loadMembersFromDB();

if ($noError)
{
	if (is_null($mg->getName()))
	{
		echo "<h3>Database contains no membership group with id " . $_GET["id"] . ".</h3>\n";
	}
	else
	{
		echo "<table border=\"1\">\n\n";
		//echo "<tr>\n<th />\n<th>Name</th>\n<th>Description</th></tr>\n";

		//$members = Member::getAllMembers();
		$members = $mg->getMembers();
		foreach ($members as $memberId => $member)
		{
			echo "<tr>\n";
			echo "<td>" . $memberId . "</td>\n";
			echo "<td>" . $member->getId() . "</td>\n";
			echo "<td>" . $member->getFirstName() . "</td>\n";
			echo "<td>" . $member->getLastName() . "</td>\n";
			echo "<td>" . $member->getEmailAddress() . "</td>\n";
			echo "</tr>\n";
		}

		echo "\n</table>\n\n";
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
