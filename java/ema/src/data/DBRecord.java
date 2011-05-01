package data;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This is the superclass of all classes containing database records.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class DBRecord implements Serializable
{
	/**
	 * Creates a new DBRecord, initializing its fields from the <code>resultSet</code> received as parameter. See
	 * {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public DBRecord(ResultSet resultSet) throws SQLException
	{
		setFromResultSet(resultSet);
	}

	public DBRecord(int id)
	{
		this.id = id;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		setId(resultSet.getInt(1));
	}

	public int getId()
	{
		return id;
	}

	public void setId(int id)
	{
		this.id = id;
	}

	private int id;
}
