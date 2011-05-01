<?php

require_once "Member.php";
require_once "ConnectionData.php";
require_once "MDB.php";

/**
 * This class supports all operations pertaining to a membership group.
 *
 * @author Bogdan Dumitriu
 * @copyright Copyright &copy; 2004, Bogdan Dumitriu
 * @version 0.1
 */
class MembershipGroup extends DBClass
{
	public function __construct($id = 0, $name = "", $description = "")
	{
		parent::__construct();

		$this->id = $id;
		$this->name = $name;
		$this->description = $description;
	}

	public function getId()
	{
		return $this->id;
	}

	public function setId($id)
	{
		$this->id = $id;
	}

	public function getName()
	{
		return $this->name;
	}

	public function setName($name)
	{
		$this->name = $name;
	}

	public function getDescription()
	{
		return $this->description;
	}

	public function setDescription($description)
	{
		$this->description = $description;
	}

	public function getMembers()
	{
		return $this->members;
	}

	/**
	 * Adds all objects in the array <var>$members</var> to the list of members of this membership group. Make
	 * sure that all objects in the array are instances of the class Member and that they have valid id's.
	 *
	 * @param array $members the array of members to be added to this membership group
	 *
	 * @return true if all objects in <var>$members</var> are instances of the class Member and have valid id's,
	 *	false otherwise
	 */
	public function addMembers($members)
	{
		if (is_null($members))
		{
			return true;
		}

		$allOk = true;
		foreach ($members as $m)
		{
			if (!($m instanceof Member))
			{
				$allOk = false;
				break;
			}
			$id = $m->getId();
			if (is_null($id) || ($id == ""))
			{
				$allOk = false;
				break;
			}
		}

		if (!$allOk)
		{
			return false;
		}

		foreach ($members as $m)
		{
			$this->members[$m->getId()] = $m;
		}

		return true;
	}

	/**
	 * Removes all objects in the array <var>$members</var> from the list of members of this membership group.
	 * Make sure that all objects in the array are instances of the class Member and that they have valid id's.
	 *
	 * @param array $members the array of members to be removed from this membership group
	 *
	 * @return true if all objects in <var>$members</var> are instances of the class Member and have valid id's,
	 *	false otherwise
	 */
	public function removeMembers($members)
	{
		if (is_null($members))
		{
			return true;
		}

		$allOk = true;
		foreach ($members as $m)
		{
			if (!($m instanceof Member))
			{
				$allOk = false;
				break;
			}
			$id = $m->getId();
			if (is_null($id) || ($id == ""))
			{
				$allOk = false;
				break;
			}
		}

		if (!$allOk)
		{
			return false;
		}

		foreach ($members as $m)
		{
			unset($this->members[$m->getId()]);
		}

		return true;
	}

	/**
	 * Creates this membership group in the database. The name and description fields of the database entry will
	 * be filled with the corresponding values of the object, while the id will be generated automatically.
	 * <br /><br />
	 * Returns true if insertion(s) completed successfully and false otherwise. You can retrieve the exact error
	 * message, as supplied by the system, by means of {@link getLastErrorMessage()}.
	 *
	 * @return boolean true if creation completed successfully, false if an error occured
	 */
	public function create()
	{
		if ($this->isConnected)
		{
			$sqlQuery = "INSERT INTO MembershipGroup VALUES (DEFAULT, '" . $this->name . "', '" .
				$this->description . "')";

			$result = $this->db->query($sqlQuery);

			if (MDB::isError($result))
			{
				$this->lastErrorMessage = $result->getMessage();
				return false;
			}

			$this->lastRowCount = $this->db->affectedRows();
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Provided that the id of this object is set to a valid id, calling this method will update the information
	 * regarding this membership group in the database. The name and description fields of the database entry will
	 * be updated to the corresponding values of the object, while the id will be left unchanged.
	 * <br /><br />
	 * Returns true if the update completed successfully and false otherwise. You can retrieve the exact error
	 * message, as supplied by the system, by means of {@link getLastErrorMessage()}. Beware that the method
	 * returns true even if no entry with its id equal to this object's id existed in the database and,
	 * consequently, no entry was deleted. You can check for this using the {@ getLastRowCount()} method.
	 *
	 * @return boolean true if update completed successfully, false if an error occured
	 */
	public function update()
	{
		if ($this->isConnected)
		{
			$sqlQuery = "UPDATE MembershipGroup SET name = '" . $this->name . "', description = '" .
				$this->description . "' WHERE id = " . $this->id;

			$result = $this->db->query($sqlQuery);

			if (MDB::isError($result))
			{
				$this->lastErrorMessage = $result->getMessage();
				return false;
			}

			$this->lastRowCount = $this->db->affectedRows();
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Provided that the id of this object is set to a valid id, calling this method will remove the entry with
	 * that id from the database. Beware that the method returns true even if no entry in the database has been
	 * deleted.
	 * <br /><br />
	 * Returns true if the delete completed successfully and false otherwise. You can retrieve the exact error
	 * message, as supplied by the system, by means of {@link getLastErrorMessage()}. Beware that the method
	 * returns true even if no entry with its id equal to this object's id existed in the database and,
	 * consequently, no entry was deleted. You can check for this using the {@ getLastRowCount()} method.
	 *
	 * @return boolean true if delete completed successfully, false if an error occured
	 */
	public function delete()
	{
		if ($this->isConnected)
		{
			$sqlQuery = "DELETE FROM MembershipGroup WHERE id = " . $this->id;

			$result = $this->db->query($sqlQuery);

			if (MDB::isError($result))
			{
				$this->lastErrorMessage = $result->getMessage();
				return false;
			}

			$this->lastRowCount = $this->db->affectedRows();
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Loads all the fields of this object from the database, provided that a valid id is set on this object
	 * before making the call to this method. Be aware that even if an entry with an id equal to the id of this
	 * object doesn't exist in the database, this method will still return true, as this is not an error. You can
	 * check whether this was the case by checking if either of {@link getName()} or {@link getDescription()}
	 * returns NULL after calling this method.
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
				$sqlQuery = "SELECT name, description FROM MembershipGroup WHERE id = " . $this->id;

				$result = $this->db->queryRow($sqlQuery);

				if (MDB::isError($result))
				{
					$this->lastErrorMessage = $result->getMessage();
					return false;
				}

				$this->lastRowCount = $this->db->affectedRows();

				$this->name = $result["name"];
				$this->description = $result["description"];

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
	 * Writes an entry to the database for each element from the <var>$members</var> array (which is a field of
	 * this class) and delets all other members of this membership group from the database. Returns true on
	 * success and false on error during access to the database or if the id of this membership group is not
	 * valid. If the <var>$member</var> array is NULL or empty, all current members are deleted and true is
	 * returned.
	 *
	 * @return boolean false on error while accessing the database or invalid id, true otherwise
	 */
	public function writeMembersToDB()
	{
		if (is_null($this->id) || ($this->id == ""))
		{
			return false;
		}

		if ($this->isConnected)
		{
			$sqlQuery = "DELETE FROM MGMembers WHERE idmg = " . $this->id;

			$result = $this->db->query($sqlQuery);

			if (MDB::isError($result))
			{
				$this->lastErrorMessage = $result->getMessage();
				return false;
			}

			$this->lastRowCount = $this->db->affectedRows();

			return $this->appendMembersToDB();
		}
		else
		{
			return false;
		}
	}

	/**
	 * Writes an entry to the database for each element from the <var>$members</var> array (which is a field of
	 * this class). Returns true on success and false on error during access to the database or if the id of this
	 * membership group is not valid. If the <var>$member</var> array is NULL or empty, nothing is written and
	 * true is returned. 
	 * <br /><br />
	 * Note: you will also get an error if you are trying to add a member which already belongs to this membership
	 * group (as it appears in the database) again.
	 *
	 * @return boolean false on error while accessing the database or invalid id, true otherwise
	 */
	public function appendMembersToDB()
	{
		if (is_null($this->id) || ($this->id == ""))
		{
			return false;
		}

		if (is_null($this->members) || count($this->members) == 0)
		{
			return true;
		}

		if ($this->isConnected)
		{
			foreach ($this->members as $m)
			{
				$sqlQuery = "INSERT INTO MGMembers VALUES (" . $this->id . ", " . $m->getId() . ")";
	
				$result = $this->db->query($sqlQuery);
	
				if (MDB::isError($result))
				{
					$this->lastErrorMessage = $result->getMessage();
					return false;
				}
	
				$this->lastRowCount = $this->db->affectedRows();
			}
	
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Loads the members of this membership groups from the database, provided that a valid id is set on this
	 * object before making the call to this method. Returns false on error and true on success. If the members
	 * array of this object contains any entries, they will be deleted before loading it with the new values.
	 * <br /><br />
	 * In case false is returned, you can retrieve the exact error message, as supplied by the system, by means of
	 * {@link getLastErrorMessage()}.
	 *
	 * @return boolean false if an error occurs while retrieving data from the database or if the id field of this
	 *	object is not set and true otherwise
	 */
	public function loadMembersFromDB()
	{
		if ($this->isConnected)
		{
			if (isset($this->id))
			{
				$sqlQuery = "SELECT idmember FROM MGMembers WHERE idmg = " . $this->id;

				$result = $this->db->query($sqlQuery);

				if (MDB::isError($result))
				{
					$this->lastErrorMessage = $result->getMessage();
					return false;
				}

				$this->lastRowCount = $this->db->affectedRows();

				// delete everything which is in the array now
				if (isset($this->members))
				{
					while (array_pop($this->members) != NULL);
				}

				$this->members = array();
				while ($row = $this->db->fetchInto($result))
				{
					$id = $row["idmember"];
					$member = new Member($id);
					$member->loadFromDB();
					$member->addToGroup($this);

					$this->members[$id] = $member;
				}
				
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
	 * Loads the values of the fields of this object from the <var>$dataSource</var> array. The array is expected
	 * to be an associative array like array("id" => <integer>, "name" => <string>, "description" => <string>).
	 * If one or more of the fields are missing from the array, the values of the corresponding fields will not be
	 * changed.
	 *
	 * @param array $dataSource the data source to load the fields of the object from
	 */
	public function loadFrom($dataSource)
	{
		if (isset($this->id))
		{
			$this->id = $dataSource["id"];
		}
		if (isset($this->name))
		{
			$this->name = $dataSource["name"];
		}
		if (isset($this->description))
		{
			$this->description = $dataSource["description"];
		}
	}

	/**
	 * Returns an associative array containing all membership groups from the database. The array will have
	 * membership group id's as keys and membership group objects as values. If an error occurs while retrieving
	 * data from the database, it will be printed directly to the output stream.
	 * <br /><br />
	 * If the table in the database is empty, an array with no elements (as opposed to NULL) will be returned.
	 *
	 * @return array the associative array described above or NULL if an error occurs while retrieving the data
	 *	from the database
	 */
	public static function getAllMGs()
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

		$sqlQuery = "SELECT * FROM MembershipGroup ORDER BY id ASC";

		$result = $db->query($sqlQuery);

		if (MDB::isError($result))
		{
			echo $result->getMessage();
			return NULL;
		}

		$output = array();
		while ($row = $db->fetchInto($result))
		{
			$id = $row["id"];
			$name = $row["name"];
			$description = $row["description"];

			$output[$id] = new MembershipGroup($id, $name, $description);
		}

		return $output;
	}

	private $id;
	private $name;
	private $description;
	
	// the list of members belonging to this membership group
	private $members;
}

?>