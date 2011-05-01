<?php

/**
 * This script handles requests to list members of a management group.
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

<h3>Members of the membership group</h3>

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
		echo "Membership group name: <i>" . $mg->getName() . "</i>.<br />";
		echo "Membership group description: <i>" . $mg->getDescription() . "</i>.<br /><br />";

		$members = $mg->getMembers();
		if (count($members) == 0)
		{
			echo "<h3>There are no members in this group.</h3>";
		}
		else
		{
			echo "<table border=\"1\">\n\n";
			echo "<tr>\n<th>First Name</th>\n<th>Last Name</th>\n<th>Email Address</th>\n</tr>\n";

			foreach ($members as $memberId => $member)
			{
				echo "<tr>\n";
				echo "<td>" . $member->getFirstName() . "</td>\n";
				echo "<td>" . $member->getLastName() . "</td>\n";
				echo "<td>" . $member->getEmailAddress() . "</td>\n";
				echo "</tr>\n";
			}

			echo "\n</table>\n\n";
		}
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
