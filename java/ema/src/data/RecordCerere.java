package data;

import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.io.Serializable;

/**
 * This class holds a record from the Cerere table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 23, 2003
 */
public class RecordCerere extends DBRecord implements Serializable
{
	/**
	 * Creates a new RecordCerere, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordCerere(ResultSet resultSet) throws SQLException
	{
		super(resultSet);
		setFromResultSet(resultSet);
	}

	public RecordCerere(int id, int cod, int an, int idTip, int idCalitate, int idSolicitant, Date dataCerere, Date dataScadenta)
	{
		super(id);
		this.cod = cod;
		this.an = an;
		this.idTip = idTip;
		this.idCalitate = idCalitate;
		this.idSolicitant = idSolicitant;
		this.dataCerere = dataCerere;
		this.dataScadenta = dataScadenta;
	}

	/**
	 * Sets the all fields from the <code>resultSet</code> received as parameter. The method expects the
	 * <code>resultSet</code> to be positioned on a valid record. It will not reposition the cursor in the
	 * <code>resultSet</code>. It also expects the record to be of the type:
	 *
	 * field 1 - the id (int)
	 * field 2 - the cod (int)
	 * field 3 - the an (int)
	 * field 4 - the id_tip (int)
	 * field 5 - the id_calitate (int)
	 * field 6 - the id_solicitant (int)
	 * field 7 - the data_cerere (java.sql.Date)
	 * field 8 - the data_scadenta (java.sql.Date)
	 *
	 * @throws SQLException if any resultSet.getXXX(...) throws it
	 */
	public void setFromResultSet(ResultSet resultSet) throws SQLException
	{
		super.setFromResultSet(resultSet);
		setCod(resultSet.getInt(2));
		setAn(resultSet.getInt(3));
		setIdTip(resultSet.getInt(4));
		setIdCalitate(resultSet.getInt(5));
		setIdSolicitant(resultSet.getInt(6));
		setDataCerere(resultSet.getDate(7));
		setDataScadenta(resultSet.getDate(8));
	}

	public int getCod()
	{
		return cod;
	}

	public void setCod(int cod)
	{
		this.cod = cod;
	}

	public int getAn()
	{
		return an;
	}

	public void setAn(int an)
	{
		this.an = an;
	}

	public int getIdTip()
	{
		return idTip;
	}

	public void setIdTip(int idTip)
	{
		this.idTip = idTip;
	}

	public int getIdCalitate()
	{
		return idCalitate;
	}

	public void setIdCalitate(int idCalitate)
	{
		this.idCalitate = idCalitate;
	}

	public int getIdSolicitant()
	{
		return idSolicitant;
	}

	public void setIdSolicitant(int idSolicitant)
	{
		this.idSolicitant = idSolicitant;
	}

	public Date getDataCerere()
	{
		return dataCerere;
	}

	public void setDataCerere(Date dataCerere)
	{
		this.dataCerere = dataCerere;
	}

	public Date getDataScadenta()
	{
		return dataScadenta;
	}

	public void setDataScadenta(Date dataScadenta)
	{
		this.dataScadenta = dataScadenta;
	}

	private int cod;
	private int an;
	private int idTip;
	private int idCalitate;
	private int idSolicitant;
	private Date dataCerere;
	private Date dataScadenta;
}
