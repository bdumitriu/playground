<?

require("config.php");

/*
 * Returns true if the row read from the .csv file was empty (i.e., the
 * .csv file contained a blank line).
 */
function rowIsEmpty($row)
{
	return (sizeof($row) == 1) && ($row[0] == "");
}

/*
 * Returns the next non-empty row or false if end of file is encountered.
 */
function getNextRow($file)
{
	global $maxRowLength;

	$row = fgetcsv($file, $maxRowLength);
	while (($row != false) && rowIsEmpty($row))
	{
		$row = fgetcsv($file, $maxRowLength);
	}

	return $row;
}

/*
 * Takes a file name ($csvFile) and returns the data contained in the file
 * if there is no error during the read process or it returns an error flag
 * ($error) and an error message along with it ($errorMsg). If $error is false,
 * then the data in $headerData, $rowData and $nrRows is valid. Otherwise,
 * it isn't.
 *
 * The maximum number of rows read from the csv file is specified by $maxRows.
 * The $nrRows variable will contain the number of rows actually read.
 */
function readCSVData($csvFile, $maxRows, &$headerData, &$rowData, &$nrRows, &$error, &$errorMsg)
{
	global $nrFields;

	// if this ends up being "true" at the end of a series of checks,
	// we have encountered some error along the way (the error message
	// will be contained in the $errorMsg variable)
	$error = false;
	$errorMsg = "";

	if (($file = @fopen($csvFile, "r")) == false)
	{
		$error = true;
		$errorMsg = "The specified file (" . $csvFile . ") could not be opened.";
		return;
	}
	else
	{
		$row = getNextRow($file);

		if ($row == false)
		{
			$error = true;
			$errorMsg = "The specified file (" . $csvFile . ") could not be read or contained no data.";
			return;
		}
		else if (sizeof($row) != $nrFields)
		{
			$error = true;
			$errorMsg = "The header row of the specified file (" . $csvFile . ") contained " .
					"either more or less than " . $nrFields . " fields.";
			return;
		}
		else
		{
			// since the fields of the header row seem to be separated
			// by ", " instead of just "," the trim function is applied to
			// all the elements of the array in order to remove the space character
			$headerData = array_map("trim", $row);
		}

		$rowNr = 0;
		while (($error == false) && ($row = getNextRow($file)) && ($rowNr < $maxRows))
		{
			if (sizeof($row) != $nrFields)
			{
				$error = true;
				$errorMsg = "Data row number " . ($rowNr+1) . " of the specified file (" . $csvFile .
						") contained either more or less than " . $nrFields . " fields.";
				return;
			}
			else
			{
				$rowData[$rowNr] = $row;
				$rowNr++;
			}
		}

		$nrRows = $rowNr;
		fclose($file);
	}
}

/*
 * WARNING! This function only works correctly for dates bewteen
 * Fri, 13 Dec 1901 20:45:54 GMT and Tue, 19 Jan 2038 03:14:07 GMT
 * (due to limitations of the php function strtotime() - which is
 * used). Additionally, by manual check, it also works for the
 * string "Jan  1 1900 12:00AM" (notice that there are two space
 * characters between Jan and 1).
 *
 * Takes a date in virtually any valid English format and transforms
 * it into the YYYY-MM-DD HH:MM:SS format and returns it.
 */
function formatDate($date)
{
	if ($date == "Jan  1 1900 12:00AM")
	{
		return "1900-01-01 00:00:00";
	}
	else
	{
		return strftime("%Y-%m-%d %H:%M:%S", strtotime($date));
	}
}

/*
 * Puts all the data from $rowData into the mysql database/table specified
 * in the configuration file (config.php). If an error occurs, the error flag
 * ($error) will be set and an error message will be put in $errorMsg. If
 * all goes well, $error will be false upon return from this function.
 *
 * The $duplicateEntries array will contain indices in the rowData array of
 * those rows which could not be successfully inserted into the database (most
 * probably because they are duplicates).
 */
function importToMySQL($csvFile, &$error, &$errorMsg, &$duplicateEntries)
{
	global $nrFields, $mysqlServer, $mysqlUser, $mysqlPassword, $mysqlDBName, $mysqlTableName;

	// if this ends up being "true" at the end of a series of checks,
	// we have encountered some error along the way (the error message
	// will be contained in the $errorMsg variable)
	$error = false;
	$errorMsg = "";

	// open the csv file
	if (($file = @fopen($csvFile, "r")) == false)
	{
		$error = true;
		$errorMsg = "The specified file (" . $csvFile . ") could not be opened.";
		return;
	}

	// create a connection to the MySQL server
	$conn = @mysql_connect($mysqlServer, $mysqlUser, $mysqlPassword);
	if (!$conn)
	{
		$error = true;
		$errorMsg = "Could not connect to the MySQL server: " . mysql_error();
		return;
	}

	if (!@mysql_select_db($mysqlDBName, $conn))
	{
		$error = true;
		$errorMsg = "Can't use the <b>" . $mysqlDBName . "</b> database: " . mysql_error();
		return;
	}

	$queryPrefix = "INSERT INTO " . $mysqlTableName . " VALUES (";
	$querySuffix = ")";

	$row = getNextRow($file);

	if ($row == false)
	{
		$error = true;
		$errorMsg = "The specified file (" . $csvFile . ") could not be read or contained no data.";
		return;
	}

	$rowNr = 0;

	// insert all entries, row by row
	while ($row = getNextRow($file))
	{
		set_time_limit(10000);
		if (sizeof($row) != $nrFields)
		{
			echo "<div class=\"text\">Error: Data row number " . ($rowNr+1) .
				" of the specified file (" . $csvFile . ") contained either more or less than " .
				$nrFields . " fields.</div><br />";
		}
		else
		{
			$query = $queryPrefix;
			for ($j = 0; $j < ($nrFields-1); $j++)
			{
				// these are the date/time fields which need to be preprocessed
				if (($j == 5) || ($j == 7) || ($j == 9) || ($j == 26) || ($j == 29)
					|| ($j == 40) || ($j == 51) || ($j == 54) || ($j == 63))
				{
					$query .= "\"" . formatDate($row[$j]) . "\"" . ", ";
				}
				else
				{
					$query .= "\"" . $row[$j] . "\"" . ", ";
				}
			}
			$query .= "\"" . formatDate($row[$nrFields-1]) . "\"" . $querySuffix;

			if (mysql_query($query, $conn) == false)
			{
				$duplicateEntries[] = $i;
			}

			$rowNr++;
		}
	}

	// close the csv file	
	fclose($file);

	// close the connection to the MySQL server
	mysql_close($conn);
}

/*
 * Tests if both dates are valid and if $startDate is before the $endDate.
 * If all is ok, true is returned, else false is returned.
 */
function validateDates(&$startDate, &$endDate, &$errorMsg)
{	
	$stTime = strtotime($startDate);
	if (($stTime == -1) || ($stTime == false))
	{
		$errorMsg = "Start date was invalid.";
		return false;
	}
	$startDate = strftime("%Y-%m-%d %H:%M:%S", $stTime);

	$endTime = strtotime($endDate);
	if (($endTime == -1) || ($endTime == false))
	{
		$errorMsg = "End date was invalid.";
		return false;
	}
	$endDate = strftime("%Y-%m-%d %H:%M:%S", $endTime);

	if ($stTime > $endTime)
	{
		$errorMsg = "Start date is after end date.";
		return false;
	}

	return true;
}

?>
