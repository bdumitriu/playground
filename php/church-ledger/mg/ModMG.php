<?php

/**
 * This script handles requests to modify a certain membership group.
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

<h3>Update information about membership group</h3>

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
?>

<form method="post" action="UpdateMG.php">
<table>
	<input type="hidden" name="id" value="<?php echo $_GET["id"]; ?>" />
	<tr>
		<td align="right">Membership group name: </td>
		<td align="left">
			<input type="text" name="name" value ="<?php echo $mg->getName(); ?>" size="30" />
		</td>
	</tr>
	<tr>
		<td align="right">Membership group description: </td>
		<td align="left">
			<input type="text" name="description" value ="<?php echo $mg->getDescription(); ?>" size="100" />
		</td>
	</tr>
	<tr>
		<td align="right">
			<input type="submit" name="updateMG" value="Save changes" />
		</td>
		<td align="left">
			<input type="reset" value="Reset changes" />
		</td>
	</tr>
</table>
</form>

<?php
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
