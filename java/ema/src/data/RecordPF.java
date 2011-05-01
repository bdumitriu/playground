package data;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the PF table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class RecordPF extends DBRecord implements Serializable
{
	/**
	 * Creates a new RecordPF, initializing its fields from the <code>resultSet</code> received as parameter. See
	 * {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws SQLException if setFromResultSet throws it
	 */
	public RecordPF(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordPF(int id, String nume, String adresa, String telefon)
	{
		super(id);
		this.nume = nume;
		this.adresa = adresa;
		this.telefon = telefon;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the nume (String)
	 * field 3 - the adresa (String)
	 * field 4 - the telefon (String)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setNume(resultSet.getString(2));
		setAdresa(resultSet.getString(3));
		setTelefon(resultSet.getString(4));
	}

	public String getNume()
	{
		return nume;
	}

	public void setNume(String nume)
	{
		this.nume = nume;
	}

	public String getAdresa()
	{
		return adresa;
	}

	public void setAdresa(String adresa)
	{
		this.adresa = adresa;
	}

	public String getTelefon()
	{
		return telefon;
	}

	public void setTelefon(String telefon)
	{
		this.telefon = telefon;
	}

	private String nume;
	private String adresa;
	private String telefon;
}
