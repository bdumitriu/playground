<?php

require_once "MembershipGroup.php";
require_once "ConnectionData.php";
require_once "DBClass.php";
require_once "MDB.php";

/**
 * This class supports all operations pertaining to a member.
 *
 * Note: some more fields & methods should be added to this class, were it not a just used for membership group
 * management.
 *
 * @author Bogdan Dumitriu
 * @copyright Copyright &copy; 2004, Bogdan Dumitriu
 * @version 0.1
 */
class Member extends DBClass
{
	public function __construct($id = 0, $lastName = "", $firstName = "", $emailAddress = "")
	{
		parent::__construct();

		$this->id = $id;
		$this->lastName = $lastName;
		$this->firstName = $firstName;
		$this->emailAddress = $emailAddress;
	}

	public function getId()
	{
		return $this->id;
	}

	public function setId($id)
	{
		$this->id = $id;
	}

	public function getLastName()
	{
		return $this->lastName;
	}

	public function setLastName($lastName)
	{
		$this->lastName = $lastName;
	}

	public function getFirstName()
	{
		return $this->firstName;
	}

	public function setFirstName($firstName)
	{
		$this->firstName = $firstName;
	}

	public function getEmailAddress()
	{
		return $this->emailAddress;
	}

	public function setEmailAddress($emailAddress)
	{
		$this->emailAddress = $emailAddress;
	}

	/**
	 * Returns an associative array containing the membership groups this member belongs to. The keys in the array
	 * are the membership groups id's, while the values are the membership group objects.
	 */
	public function getMemberOf()
	{
		return $this->members;
	}

	/**
	 * Adds <var>$membershipGroup</var> to the list of membership groups this member belongs to. Warning: the id
	 * of the membership group should be properly set before calling this method.
	 *
	 * @param MembershipGroup $membershipGroup the membership group to add to the list of membership groups this
	 *	member belongs to
	 *
	 * @return boolean true if the id of $membershipGroup is valid and false otherwise
	 */
	public function addToGroup($membershipGroup)
	{
		$id = $membershipGroup->getId();
		if (is_null($id) || ($id == ""))
		{
			return false;
		}
		
		$this->memberOf[$id] = $membershipGroup;
		
		return true;
	}

	/**
	 * Removess <var>$membershipGroup</var> from the list of membership groups this member belongs to. Warning:
	 * the id of the membership group should be properly set before calling this method.
	 *
	 * @param MembershipGroup $membershipGroup the membership group to remove from the list of membership groups
	 *	this member belongs to
	 *
	 * @return boolean true if the id of $membershipGroup is valid and false otherwise
	 */
	public function removeFromGroup($membershipGroup)
	{
		$id = $membershipGroup->getId();
		if (is_null($id) || ($id == ""))
		{
			return false;
		}

		unset($this->memberOf[$id]);
		
		return true;
	}

	/**
	 * Loads all the fields of this object from the database, provided that a valid id is set on this object
	 * before making the call to this method. Be aware that even if an entry with an id equal to the id of this
	 * object doesn't exist in the database, this method will still return true, as this is not an error. You can
	 * check whether this was the case by checking if, for example, {@link getLastName()} returns NULL after
	 * calling this method.
	 * <br /><br />
	 * In case false is returned, you can retrieve the exact error message, as supplied by the system, by means of
	 * {@link getLastErrorMessage()}.
	 *
	 * @return boolean false if an error occurs while retrieving data from the database or if the id field of this
	 *	object is not set and true otherwise
	 */
	public function loadFromDB()
	{
		if ($this->isConnected)
		{
			if (isset($this->id))
			{
				$sqlQuery = "SELECT lastname, firstname, email FROM Member WHERE memberid = " . $this->id;

				$result = $this->db->queryRow($sqlQuery);

				if (MDB::isError($result))
				{
					$this->lastErrorMessage = $result->getMessage();
					return false;
				}

				$this->lastRowCount = $this->db->affectedRows();

				$this->lastName = $result["lastname"];
				$this->firstName = $result["firstname"];
				$this->emailAddress= $result["email"];

				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	/**
	 * Returns an associative array containing all members from the database, except those which belong to the
	 * membership group identified by <var>$id</var>. The array will have member id's as keys and member objects
	 * as values. If an error occurs while retrieving data from the database, it will be printed directly to the
	 * output stream.
	 * <br /><br />
	 * If no such members exist, an array with no elements (as opposed to NULL) will be returned.
	 *
	 * @param int $id the id of the membership group whose members should be excluded from the selection
	 *
	 * @return array the associative array described above or NULL if an error occurs while retrieving the data
	 *	from the database
	 */
	public static function getAvailableMembersFor($id)
	{
		global $user, $pass, $protocol, $host, $dbName;

		// the data source name
		$dsn = "pgsql://$user:$pass@$protocol+$host/$dbName";

		// connect to the database
		$db = MDB::connect($dsn);

		// check for error in opening the connection
		if (MDB::isError($db))
		{
			echo $db->getMessage();
			return NULL;
		}

		// set the default fetchmode to MDB_FETCHMODE_ASSOC
		$db->setFetchMode(MDB_FETCHMODE_ASSOC);

		$sqlQuery = "SELECT memberid, lastname, firstname, email FROM Member WHERE memberid NOT IN (SELECT " .
			"idmember FROM MGMembers WHERE idmg = " . $id . ") ORDER BY memberid ASC";

		$result = $db->query($sqlQuery);

		if (MDB::isError($result))
		{
			echo $result->getMessage();
			return NULL;
		}

		$output = array();
		while ($row = $db->fetchInto($result))
		{
			$id = $row["memberid"];
			$lastName = $row["lastname"];
			$firstName = $row["firstname"];
			$emailAddress = $row["email"];

			$output[$id] = new Member($id, $lastName, $firstName, $emailAddress);
		}

		return $output;
	}

	/**
	 * Returns an associative array containing all members from the database. The array will have member id's as
	 * keys and member objects as values. If an error occurs while retrieving data from the database, it will be
	 * printed directly to the output stream.
	 * <br /><br />
	 * If the table in the database is empty, an array with no elements (as opposed to NULL) will be returned.
	 *
	 * @return array the associative array described above or NULL if an error occurs while retrieving the data
	 *	from the database
	 */
	public static function getAllMembers()
	{
		global $user, $pass, $protocol, $host, $dbName;

		// the data source name
		$dsn = "pgsql://$user:$pass@$protocol+$host/$dbName";

		// connect to the database
		$db = MDB::connect($dsn);

		// check for error in opening the connection
		if (MDB::isError($db))
		{
			echo $db->getMessage();
			return NULL;
		}

		// set the default fetchmode to MDB_FETCHMODE_ASSOC
		$db->setFetchMode(MDB_FETCHMODE_ASSOC);

		$sqlQuery = "SELECT memberid, lastname, firstname, email FROM Member ORDER BY memberid ASC";

		$result = $db->query($sqlQuery);

		if (MDB::isError($result))
		{
			echo $result->getMessage();
			return NULL;
		}

		$output = array();
		while ($row = $db->fetchInto($result))
		{
			$id = $row["memberid"];
			$lastName = $row["lastname"];
			$firstName = $row["firstname"];
			$emailAddress = $row["email"];

			$output[$id] = new Member($id, $lastName, $firstName, $emailAddress);
		}

		return $output;
	}

	private $id;
	private $lastName;
	private $firstName;
	private $emailAddress;

	// the list of membership groups to which this member belongs
	private $memberOf;
}

?>