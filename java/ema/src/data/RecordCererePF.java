package data;

import java.io.Serializable;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * This class holds a record from the CererePF table.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 23, 2003
 */
public class RecordCererePF extends RecordCerere implements Serializable
{
	/**
	 * Creates a new RecordCererePF, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordCererePF(ResultSet resultSet, int idPf) throws SQLException
	{
		super(resultSet);
		this.idPf = idPf;
	}

	public RecordCererePF(int id, int cod, int an, int idTip, int idCalitate, int idSolicitant, Date dataCerere,
		Date dataScadenta, int idPf)
	{
		super(id, cod, an, idTip, idCalitate, idSolicitant, dataCerere, dataScadenta);
		this.idPf = idPf;
	}

	public int getIdPf()
	{
		return idPf;
	}

	public void setIdPf(int idPf)
	{
		this.idPf = idPf;
	}

	private int idPf;
}
