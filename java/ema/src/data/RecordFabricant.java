package data;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the Fabricant table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class RecordFabricant extends DBRecord
{
	/**
	 * Creates a new RecordFabricant, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordFabricant(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordFabricant(int id, String nume)
	{
		super(id);
		this.nume = nume;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the nume (String)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setNume(resultSet.getString(2));
	}

	public String getNume()
	{
		return nume;
	}

	public void setNume(String nume)
	{
		this.nume = nume;
	}

	private String nume;
}
