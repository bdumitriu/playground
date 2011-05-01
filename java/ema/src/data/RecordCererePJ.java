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
public class RecordCererePJ extends RecordCerere implements Serializable
{
	/**
	 * Creates a new RecordCererePJ, initializing its fields from the <code>resultSet</code> received as parameter.
	 * See {@link #setFromResultSet setFromResultSet} for details.
	 *
	 * @throws java.sql.SQLException if setFromResultSet throws it
	 */
	public RecordCererePJ(ResultSet resultSet, int idContract) throws SQLException
	{
		super(resultSet);
		this.idContract = idContract;
	}

	public RecordCererePJ(int id, int cod, int an, int idTip, int idCalitate, int idSolicitant, Date dataCerere,
		Date dataScadenta, int idContract)
	{
		super(id, cod, an, idTip, idCalitate, idSolicitant, dataCerere, dataScadenta);
		this.idContract = idContract;
	}

	public int getIdContract()
	{
		return idContract;
	}

	public void setIdContract(int idContract)
	{
		this.idContract = idContract;
	}

	private int idContract;
}
