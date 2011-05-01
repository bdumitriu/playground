package data;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the DN table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class RecordDN extends DBRecord
{
	/**
	 * Creates a new RecordDN, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordDN(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordDN(int id, int numar)
	{
		super(id);
		this.numar = numar;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the numar (int)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setNumar(resultSet.getInt(2));
	}

	public int getNumar()
	{
		return numar;
	}

	public void setNumar(int numar)
	{
		this.numar = numar;
	}

	private int numar;
}