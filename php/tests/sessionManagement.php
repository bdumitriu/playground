<?
	session_start();
	session_register('SESSION');
	if (!isset($SESSION))
	{
		$SESSION['count'] = 0;
?>
<li>Count initialized. Please reload this page to see it increment.
<?
	}
	else
	{
		echo '<li>Waking up session ';
		echo $PHPSESSID;
		echo '.';
		$SESSION['count']++;
	}
	echo ' The counter is now ';
	echo $SESSION['count'];
	echo '.';
?>
