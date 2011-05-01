<?php

/**
 * This script handles requests to select members to add to a management group.
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

<h3>Add members to membership group</h3>

<?php

$mg->setId($_GET["id"]);;

if ($mg->loadFromDB())
{
	if (is_null($mg->getName()))
	{
		echo "<h3>Database contains no membership group with id " . $_GET["id"] . ".</h3>\n";
	}
	else
	{
		echo "Membership group name: <i>" . $mg->getName() . "</i>.<br />";
		echo "Membership group description: <i>" . $mg->getDescription() . "</i>.<br /><br />";

		$members = Member::getAvailableMembersFor($mg->getId());
		if (count($members) == 0)
		{
			echo "<h3>There are no (more) members you can add to this group.</h3>";
		}
		else
		{
			echo "<form action=\"AddMembers.php\" method=\"post\">\n\n";
			echo "<input type=\"hidden\" name=\"id\" value=\"" . $mg->getId() . "\" />\n\n";
			echo "<table border=\"1\">\n\n";
			echo "<tr>\n<th />\n<th>First Name</th>\n<th>Last Name</th>\n<th>Email Address</th>\n</tr>\n";

			foreach ($members as $memberId => $member)
			{
				echo "<tr>\n";
				echo "<td><input type=\"checkbox\" name=\"" . $memberId . "\" /></td>\n";
				echo "<td>" . $member->getFirstName() . "</td>\n";
				echo "<td>" . $member->getLastName() . "</td>\n";
				echo "<td>" . $member->getEmailAddress() . "</td>\n";
				echo "</tr>\n";
			}

			echo "\n</table>\n\n";
			echo "<input type=\"submit\" name=\"addMembers\" value=\"Add selected\" />\n\n";
			echo "</form>\n\n";
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
