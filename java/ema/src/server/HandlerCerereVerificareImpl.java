package server;

import data.*;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.ResultSet;

import shared.EMALogger;

/**
 * Provides an implementation of the HandlerCerereVerificare interface.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class HandlerCerereVerificareImpl
        extends UnicastRemoteObject
	implements HandlerCerereVerificare, Serializable
{
	public HandlerCerereVerificareImpl() throws RemoteException
	{}
	
	public boolean checkCodCerere(RecordCerere record)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		boolean result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call checkCodCerere(?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.BIT);
			stmt.setInt(2, record.getCod());
			stmt.setInt(3, record.getAn());

			EMALogger.getInstance().logOtherMessage("Calling function checkCodCerere(" + record.getCod() +
				", " + record.getAn() + ").");
			stmt.execute();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			result = stmt.getBoolean(1);
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return false;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	public RecordTipCerere[] getTipuriCerere() throws RemoteException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordTipCerere[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getTipuriCerere()}");

			EMALogger.getInstance().logOtherMessage("Calling function getTipuriCerere().");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordTipCerere[size];
			}
			else
			{
				DBInterfaceManager.getInstance().close(conn);
				return null;
			}

			int index = 0;
			rs.beforeFirst();
			rs.next();

			while (!rs.isAfterLast())
			{
				result[index++] = new RecordTipCerere(rs);
				rs.next();
			}
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return null;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	public RecordSolicitant[] getSolictianti() throws RemoteException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordSolicitant[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getSolicitanti()}");

			EMALogger.getInstance().logOtherMessage("Calling function getSolicitanti().");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordSolicitant[size];
			}
			else
			{
				DBInterfaceManager.getInstance().close(conn);
				return null;
			}

			int index = 0;
			rs.beforeFirst();
			rs.next();

			while (!rs.isAfterLast())
			{
				result[index++] = new RecordSolicitant(rs);
				rs.next();
			}
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return null;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	public RecordCalitate[] getCalitati() throws RemoteException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordCalitate[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getCalitati()}");

			EMALogger.getInstance().logOtherMessage("Calling function getCalitati().");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordCalitate[size];
			}
			else
			{
				DBInterfaceManager.getInstance().close(conn);
				return null;
			}

			int index = 0;
			rs.beforeFirst();
			rs.next();

			while (!rs.isAfterLast())
			{
				result[index++] = new RecordCalitate(rs);
				rs.next();
			}
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return null;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	/**
	 * Inserts all the Apometru's in <code>apometre</code> in the Apometru table and the inserts an entry for each
	 * of these Apometru's in the CerereApometru table in association with <code>idCerere</code>.
	 *
	 * @param apometre the records containing all Apometru's data
	 * @param idCerere the id of the Cerere to associate all the Apometru's with
	 * @return 0 on success, -1 on failure
	 */
	protected int insertApometre(RecordApometru[] apometre, int idCerere)
	{
		int idApometru;
		for (int i = 0; i < apometre.length; i++)
		{

			if ((idApometru = insertApometru(apometre[i])) == -1)
			{
				return -1;
			}
			if (insertCerereApometru(idCerere, idApometru) == -1)
			{
				return -1;
			}
		}

		return 0;
	}

	/**
	 * Inserts a new Apometru into the Apometru table.
	 *
	 * @param record the record containing all Apometru data
	 * @return the id of the newly inserted Apometru or -1 in case of failure
	 */
	private int insertApometru(RecordApometru record)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call insertApometru(?,?,?,?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);
			stmt.setInt(2, record.getIdFabricant());
			stmt.setString(3, record.getSerie());
			stmt.setInt(4, record.getIdDn());
			stmt.setString(5, record.getClasa());
			stmt.setInt(6, record.getIdDomeniu());
			stmt.setString(7, record.getObs());

			EMALogger.getInstance().logOtherMessage("Calling function insertApometru(" +
				record.getIdFabricant() + ", " + record.getSerie() + ", " + record.getIdDn() + ", " +
				record.getClasa() + ", " + record.getIdDomeniu() + "," + record.getObs() + ").");
			stmt.execute();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			result = stmt.getInt(1);
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return -1;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	/**
	 * Inserts a new CerereApometru into the CerereApometru table.
	 *
	 * @param idCerere the id of the Cerere
	 * @param idApometru the id of the Apometru
	 * @return 0 on success, -1 on failure
	 */
	private int insertCerereApometru(int idCerere, int idApometru)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();

		try
		{
			CallableStatement stmt = conn.prepareCall("{call insertCerereApometru(?,?)}");
			stmt.setInt(1, idCerere);
			stmt.setInt(2, idApometru);

			EMALogger.getInstance().logOtherMessage("Calling function insertCerereApometru(" + idCerere +
				", " + idApometru + ").");
			stmt.execute();
			EMALogger.getInstance().logOtherMessage("Function call successful.");
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
			return -1;
		}

		DBInterfaceManager.getInstance().close(conn);
		return 0;
	}
}
