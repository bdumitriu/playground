package server;

import data.RecordPF;
import data.RecordApometru;
import data.RecordCerere;

import java.rmi.RemoteException;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.ResultSet;

import shared.EMALogger;

/**
 * Provides an implementation of the HandlerCerereVerificarePF interface.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public class HandlerCerereVerificarePFImpl
	extends HandlerCerereVerificareImpl
	implements HandlerCerereVerificarePF, Serializable
{
	public HandlerCerereVerificarePFImpl() throws RemoteException
	{}

	public int insertCerereVerificare(RecordCerere cerere, RecordPF pf, RecordApometru[] apometre)
	        throws RemoteException
	{
		/*
		System.out.println(cerere.getCod() + "/" + cerere.getAn() + ": (" + cerere.getIdCalitate() + ", " +
			cerere.getIdSolicitant() + ", " + cerere.getIdTip() + ") - din " + cerere.getDataCerere() +
			" scadenta: " + cerere.getDataScadenta());

		System.out.println(pf.getNume() + ", " + pf.getAdresa() + ", " + pf.getTelefon());

		for (int i = 0; i < apometre.length; i++)
		{
			RecordApometru a = apometre[i];
			System.out.println(a.getSerie() + "(" + a.getIdFabricant() + "): " + "(" + a.getIdDn() + "," +
				a.getIdDomeniu() + "), clasa " + a.getClasa() + "obs. " + a.getObs());
		}
		*/

		// check whether a person with the same record already exists in the database
		// the checking is done in the SQL code
		int idPF;
		if ((idPF = insertPF(pf)) == -1)
		{
			// no logging is done since an error message has already been logged
			return -1;
		}

		// try to insert a new Cerere (request)
		int idCererePF;
		if ((idCererePF = insertCererePF(cerere, idPF)) == -1)
		{
			// no logging is done since an error message has already been logged
			return -1;
		}

		if (insertApometre(apometre, idCererePF) == -1)
		{
			// no logging is done since an error message has already been logged
			return -1;
		}

		// return 0 on success
		return 0;
	}

	public boolean checkPF(RecordPF record)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		boolean result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call checkPF(?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.BIT);
			stmt.setString(2, record.getNume());
			stmt.setString(3, record.getAdresa());
			stmt.setString(4, record.getTelefon());

			EMALogger.getInstance().logOtherMessage("Calling function checkPF(" + record.getNume() +
				", " + record.getAdresa() + ", " + record.getTelefon() + ").");
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

	public RecordPF[] getNamesLike(String likePattern)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordPF[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getNamesLike(?)}");
			stmt.setString(1, likePattern);

			EMALogger.getInstance().logOtherMessage("Calling function getNamesLike('" + likePattern + "').");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordPF[size];
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
				result[index++] = new RecordPF(rs);
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
	 * Returns the id of the PF with the given record.
	 *
	 * @param record the record identifying the PF
	 * @return the id if such a PF is found in the PF table or -1 if the PF is not found or if an error occurs
	 */
	private int getPFFor(RecordPF record) throws SQLException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call getPFFor(?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);
			stmt.setString(2, record.getNume());
			stmt.setString(3, record.getAdresa());
			stmt.setString(4, record.getTelefon());

			EMALogger.getInstance().logOtherMessage("Calling function getPFFor(" + record.getNume() +
				", " + record.getAdresa() + ", " + record.getTelefon() + ").");
			stmt.execute();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			result = stmt.getInt(1);
		}
		catch (SQLException e)
		{
			DBInterfaceManager.getInstance().close(conn);
			throw e;
		}

		DBInterfaceManager.getInstance().close(conn);
		return result;
	}

	/**
	 * Inserts a new PF into the PF table.
	 *
	 * @param record the record containing all PF data
	 * @return the id of the newly inserted PF or -1 in case of failure
	 */
	private int insertPF(RecordPF record)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call insertPF(?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);
			stmt.setString(2, record.getNume());
			stmt.setString(3, record.getAdresa());
			stmt.setString(4, record.getTelefon());

			EMALogger.getInstance().logOtherMessage("Calling function insertPF(" + record.getNume() +
				", " + record.getAdresa() + ", " + record.getTelefon() + ").");
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
	 * Inserts a new CererePF into the CererePF table.
	 *
	 * @param record the record containing all Cerere data
	 * @param idPF the id of the PF making the Cerere (request)
	 * @return the id of the newly inserted CererePF or -1 in case of failure
	 */
	private int insertCererePF(RecordCerere record, int idPF)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call insertCererePF(?,?,?,?,?,?,?,?)}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);
			stmt.setInt(2, record.getCod());
			stmt.setInt(3, record.getAn());
			stmt.setInt(4, record.getIdTip());
			stmt.setInt(5, record.getIdCalitate());
			stmt.setInt(6, record.getIdSolicitant());
			stmt.setDate(7, record.getDataCerere());
			stmt.setDate(8, record.getDataScadenta());
			stmt.setInt(9, idPF);

			EMALogger.getInstance().logOtherMessage("Calling function insertCererePF(" + record.getCod() +
				", " + record.getAn() + ", " + record.getIdTip() + ", " + record.getIdCalitate() + ", "
				+ record.getIdSolicitant() + "," + record.getDataCerere() + "," +
				record.getDataScadenta() + "," + idPF + ").");
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
}