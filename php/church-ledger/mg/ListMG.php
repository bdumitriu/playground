<?php

/**
 * This script handles requests for listing all the membership groups.
 */

require_once "ConnectionData.php";
require_once "MembershipGroup.php";

?>

<html>

<head>
<title>Church Ledger - Membership Group Management Module</title>
</head>

<body>

<?php

if (!is_null($mgs = MembershipGroup::getAllMGs()))
{
	echo "<h3>List of available membership groups</h3>\n\n";

	if (count($mgs) > 0)
	{
		echo "<form action=\"DeleteMG.php\" method=\"post\">\n\n";
		echo "<table border=\"1\">\n\n";
		echo "<tr>\n<th />\n<th>Name</th>\n<th>Description</th><th colspan=\"3\">Actions</tr>\n";

		foreach ($mgs as $mg)
		{
			$name = $mg->getName();
			if (is_null($name) || $name == "")
			{
				$name = "N/A";
			}
			$description = $mg->getDescription();
			if (is_null($description) || $description == "")
			{
				$description = "N/A";
			}

			echo "<tr>\n";
			echo "<td><input type=\"checkbox\" name=\"" . $mg->getId() . "\" /></td>\n";
			echo "<td><a href=\"ModMG.php?id=" . $mg->getId() . "\">" . $name . "</a></td>\n";
			echo "<td><a href=\"ModMG.php?id=" . $mg->getId() . "\">" . $description . "</a></td>\n";
			echo "<td>&gt; <a href=\"ListMembers.php?id=" . $mg->getId() . "\">list members</a> &lt;</td>\n";
			echo "<td>&gt; <a href=\"SelectAddMembers.php?id=" . $mg->getId() . "\">add members</a> &lt;</td>\n";
			echo "<td>&gt; <a href=\"SelectRemoveMembers.php?id=" . $mg->getId() . "\">remove members</a> &lt;</td>\n";
			echo "</tr>\n";
		}

		echo "\n</table>\n\n";
		echo "<input type=\"submit\" name=\"deleteMG\" value=\"Delete selected\" />";
		echo "</form>\n\n";
	}
	else
	{
		echo "Currently, there are no membership groups defined.\n\n";
	}
	echo "<br /><br />\nNote: please refresh this page if you have performed any type of modification and it appears like nothing has changed.\n";
}
else
{
	echo "<h3>Error in retrieving list of membership groups.</h3>\n";
}

?>

<br /><br />
Back to <a href="mg.html">membership group management</a>.

</body>

</html>
