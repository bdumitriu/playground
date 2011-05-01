<?php

/*
 * Author:   Bogdan DUMITRIU
 * Date:     July 2001
 * Version:  1.0
 * Function: Processes the info supplied by a web page. 
 *	     The processing adds some information in a mysql database.
 */

	/*
	 * First, set PHP's error reporting level to 0 so that the user
	 * cannot see raw error messages (could be insecure). This way
	 * the only message the user will see is "An error has occured.
	 * Check server's error log for further information." which is
	 * defined below in the badStuff function. The Apache server
	 * administrator can see the error message in the server's
	 * error log file.
	 */
	error_reporting(0);
	
	/*
	 * The host, username and password used for connecting to mysqld
	 * and the database name to use.
	 */
	$host = "localhost";
	$user = "lt";
	$pass = "web";
	$database = "lt_web";
	
	/*
	 * Some default error messages...
	 */
	$connectionError = "Could not connect to mysqld. Check if mysqld is running and, if so, check the privileges of user `$user'.";
	$useError = "Could not make `$database' the default database. Check if `$database' database exists and if user `$user' is allowed to use it.";
	$selectError = "Could not execute select query. Check if user `$user' has select rights on the tables of the `$database' database.";
	$insertError = "Could not execute insert query. Check if user `$user' has insert rights on the tables of the `$database' database.";
	$unexpectedError = "Some undefined error occured. Execution stopped. Please check data integrity manually in the `$database' database.";
	
	/*
	 * ...and a function to deal with them.
	 */
	function badStuff($message)
	{
		error_log($message, 0);
		die("An error has occured. Check server's error log for further information.");
	}

	/*
	 * Connect to mysql server and make $database the working database.
	 */	
	$link = mysql_connect($host, $user, $pass) or badStuff($connectionError);
	mysql_select_db($database) or badStuff($useError);
	
	/*
	 * If after the following block $id is -1 then the user is not
	 * registered in the database. If $id holds a positive value
	 * then the user is registered and this positive value is its
	 * userid.
	 */
	$query = "select id from clienti where nume='$nume' and email='$email'";
	$result = mysql_query($query) or badStuff($selectError);
	if (mysql_num_rows($result) == 1)
	{
		$id = mysql_result($result, 0);
	}
	elseif (mysql_num_rows($result) > 1)
	{
		badStuff($unexpectedError);
	}
	else
	{
		$id = -1;
	}
	mysql_free_result($result);
	
	/*
	 * Build & execute the insert query depending on the fields supplied
	 * by the web user. This only happens if we don't already have the
	 * user in our database (i.e. $id = -1).
	 */
	if ($id == -1)
	{
		/*
		 * Build the query.
		 */
		$query = "insert into clienti (id, nume, email";
		$values = ") values (NULL, '$nume', '$email'";
		if ($tel != "")
		{
			$query .= ", telefon";
			$values .= ", " . "'$tel'";
		}
		if ($firma != "")
		{
			$query .= ", firma";
			$values .= ", " . "'$firma'";
		}
		if ($telFirma != "")
		{
			$query .= ", telfirma";
			$values .= ", " . "'$telFirma'";
		}
		if ($emailFirma != "")
		{
			$query .= ", emailfirma";
			$values .= ", " . "'$emailFirma'";
		}
		if ($web != "")
		{
			$query .= ", web";
			$values .= ", " . "'$web'";
		}
		if ($venit != "")
		{
			$query .= ", venit";
			$values .= ", " . "'$venit'";
		}
		if ($cifra != "")
		{
			$query .= ", cifraaf";
			$values .= ", " . "'$cifra'";
		}
		if ($dom != "")
		{
			$query .= ", domeniu";
			$values .= ", " . "'$dom'";
		}
		if ($oras != "")
		{
			$query .= ", oras";
			$values .= ", " . "'$oras'";
		}
		$query .= $values . ")";
		
		/*
		 * Execute the query and refresh the $id value.
		 */
		mysql_query($query) or badStuff($insertError);
		if (mysql_affected_rows() != 1)
			badStuff($unexpectedError);
		$result = mysql_query("select LAST_INSERT_ID()") or badStuff($selectError);
		if (mysql_num_rows($result) != 1)
		{	
			badStuff($unexpectedError);
		}
		else
		{
			$id = mysql_result($result, 0);
		}
		mysql_free_result($result);
	}
	
	/*
	 * Now we have a valid $id so we can go on to filling the rest of
	 * the information in the other table. First we repeat the process
	 * of building the query depending on user input.
	 */
	$query = "insert into calcule (id, tipprodus";
	$values = ") values ('$id', '$tipProdus'";
	if ($durataContract != "")
	{
		$query .= ", durata";
		$values .= ", " . "'$durataContract'";
	}
	if ($sumaCalculata != "")
	{
		$query .= ", suma";
		$values .= ", " . "'$sumaCalculata'";
	}
	if ($tva != "")
	{
		$query .= ", tva";
		$values .= ", " . "'$tva'";
	}
	if ($avans != "")
	{
		$query .= ", avans";
		$values .= ", " . "'$avans'";
	}
	$query .= ", data" . $values . ", current_date)";
	
	/*
	 * Now we execute the previously built query
	 */
	mysql_query($query) or badStuff($insertError);
	if (mysql_affected_rows() != 1)
		badStuff($unexpectedError);
		
	/*
	 * All done
	 */
?>
