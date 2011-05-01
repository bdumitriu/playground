package data;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the TipCerere table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class RecordTipCerere extends DBRecord
{
	/**
	 * Creates a new RecordTipCerere, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordTipCerere(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordTipCerere(int id, String nume, int valabilitate)
	{
		super(id);
		this.nume = nume;
		this.valabilitate = valabilitate;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the nume (String)
	 * field 3 - the valabilitate (int)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setNume(resultSet.getString(2));
		setValabilitate(resultSet.getInt(3));
	}

	public String getNume()
	{
		return nume;
	}

	public void setNume(String nume)
	{
		this.nume = nume;
	}

	public int getValabilitate()
	{
		return valabilitate;
	}

	public void setValabilitate(int valabilitate)
	{
		this.valabilitate = valabilitate;
	}

	private String nume;
	private int valabilitate;
}
