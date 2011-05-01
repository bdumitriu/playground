package data;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the Apometru table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class RecordApometru extends DBRecord
{
	/**
	 * Creates a new RecordApometru, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordApometru(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordApometru(int id, int idFabricant, String serie, int idDn, String clasa, int idDomeniu, String obs)
	{
		super(id);
		this.idFabricant = idFabricant;
		this.serie = serie;
		this.idDn = idDn;
		this.clasa = clasa;
		this.idDomeniu = idDomeniu;
		this.obs = obs;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the id_fabricant (int)
	 * field 3 - the serie (String)
	 * field 4 - the id_dn (int)
	 * field 5 - the clasa (String)
	 * field 6 - the id_domeniu (int)
	 * field 7 - the observatii (String)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setIdFabricant(resultSet.getInt(2));
		setSerie(resultSet.getString(3));
		setIdDn(resultSet.getInt(4));
		setClasa(resultSet.getString(5));
		setIdDomeniu(resultSet.getInt(6));
		setObs(resultSet.getString(7));
	}

	public int getIdFabricant()
	{
		return idFabricant;
	}

	public void setIdFabricant(int idFabricant)
	{
		this.idFabricant = idFabricant;
	}

	public String getSerie()
	{
		return serie;
	}

	public void setSerie(String serie)
	{
		this.serie = serie;
	}

	public int getIdDn()
	{
		return idDn;
	}

	public void setIdDn(int idDn)
	{
		this.idDn = idDn;
	}

	public String getClasa()
	{
		return clasa;
	}

	public void setClasa(String clasa)
	{
		this.clasa = clasa;
	}

	public int getIdDomeniu()
	{
		return idDomeniu;
	}

	public void setIdDomeniu(int idDomeniu)
	{
		this.idDomeniu = idDomeniu;
	}

	public String getObs()
	{
		return obs;
	}

	public void setObs(String obs)
	{
		this.obs = obs;
	}

	private int idFabricant;
	private String serie;
	private int idDn;
	private String clasa;
	private int idDomeniu;
	private String obs;
}
