<?php

require_once "ConnectionData.php";
require_once "MDB.php";

/**
 * This class is the base class for all classes which need some sort of database access. Although possible, there
 * isn't really much use in instantiating this class directly.
 *
 * @author Bogdan Dumitriu
 * @copyright Copyright &copy; 2004, Bogdan Dumitriu
 * @version 0.1
 */
class DBClass
{
	function DBClass()
	{
		global $user, $pass, $protocol, $host, $dbName;

		// the data source name
		$dsn = "pgsql://$user:$pass@$protocol+$host/$dbName";

		// connect to the database
		$db = MDB::connect($dsn);

		// check for error in opening the connection
		if (MDB::isError($db))
		{
			$this->isConnected = false;
			$this->lastErrorMessage = $db->getMessage();
		}
		else
		{
			$this->isConnected = true;
			$this->lastErrorMessage = "";
			
			// set the default fetchmode to MDB_FETCHMODE_ASSOC
			$db->setFetchMode(MDB_FETCHMODE_ASSOC);
	
			$this->db = $db;
		}
		
		$this->lastRowCount = -1;
	}


	/**
	 * Returns the last error message received during the execution of any of the methods of this class. You can
	 * use this method to provide the user with a more detailed description in case of an error.
	 *
	 * @return string the last error message received from the system
	 */
	function getLastErrorMessage()
	{
		return $this->lastErrorMessage;
	}

	/**
	 * Returns the last row count resulted during the _successful_ execution of any of the methods of this class.
	 * You can use this method to check for the effect of the various operations you perform.
	 *
	 * @return int the last row count resulted
	 */
	function getLastRowCount()
	{
		return $this->lastRowCount;
	}

	var $db;
	var $lastErrorMessage;
	var $lastRowCount;
	var $isConnected;
}

?>