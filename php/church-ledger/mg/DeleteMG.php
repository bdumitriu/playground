<?php

/**
 * This script handles requests to delete membership groups.
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

$count = 0;
$wasError = false;
foreach ($_POST as $key => $value)
{
	if ($value == "on")
	{
		$mg->setId($key);

		// first, delete all entries about members of this group
		// (just in case there's no ON DELETE CASCADE set up for the DB)
		if (!($mg->writeMembersToDB()))
		{
			$wasError = true;
			break;
		}

		if ($mg->delete())
		{
			$count += $mg->getLastRowCount();
		}
		else
		{
			$wasError = true;
			break;
		}
	}
}

if ($wasError)
{
	if ($count == 0)
	{
		echo "<h3>Error while deleting membership group(s). No groups have been deleted so far.</h3>\n";
	}
	else if ($count == 1)
	{
		echo "<h3>Error while deleting membership group(s). 1 group has already been deleted.</h3>\n";
	}
	else
	{
		echo "<h3>Error while deleting membership group(s). " . $count . " groups have already been deleted.</h3>\n";
	}
	echo "<h4>Error message was: " . $mg->getLastErrorMessage() . "</h4>\n\n";
}
else
{
	if ($count == 0)
	{
		echo "<h3>No membership group was selected for deletion.</h3>\n";
	}
	else if ($count == 1)
	{
		echo "<h3>1 membership group was successfully deleted.</h3>\n";
	}
	else
	{
		echo "<h3>" . $count . " membership groups were successfully deleted.</h3>\n";
	}
}
?>

<br /><br />
Back to <a href="ListMG.php">listing</a> of membership groups.

</body>

</html>