package server;

import data.RecordFabricant;
import data.RecordDN;
import data.RecordDomeniu;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.io.Serializable;
import java.sql.Connection;
import java.sql.CallableStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import shared.EMALogger;

/**
 * Provides an implementation of the HandlerCerereApometre interface.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class HandlerCerereApometreImpl
	extends UnicastRemoteObject
	implements HandlerCerereApometre, Serializable
{
	public HandlerCerereApometreImpl() throws RemoteException
	{}

	public RecordFabricant[] getFabricanti()
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordFabricant[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getFabricanti()}");

			EMALogger.getInstance().logOtherMessage("Calling function getFabricanti().");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordFabricant[size];
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
				result[index++] = new RecordFabricant(rs);
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

	public void insertFabricant(RecordFabricant fabricant)
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();

		try
		{
			CallableStatement stmt = conn.prepareCall("{call insertFabricant(?)}");
			stmt.setString(2, fabricant.getNume());

			EMALogger.getInstance().logOtherMessage("Calling function insertFabricant(" +
				fabricant.getNume() + ").");
			stmt.executeUpdate();
			EMALogger.getInstance().logOtherMessage("Function call successful.");
		}
		catch (SQLException e)
		{
			EMALogger.getInstance().logDefaultSQLExceptionMessage(e);
			DBInterfaceManager.getInstance().close(conn);
		}

		DBInterfaceManager.getInstance().close(conn);
	}

	public RecordDN[] getDNs()
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		RecordDN[] result = null;

		try
		{
			CallableStatement stmt = conn.prepareCall("{call getDNs()}");

			EMALogger.getInstance().logOtherMessage("Calling function getDNs().");
			ResultSet rs = stmt.executeQuery();
			EMALogger.getInstance().logOtherMessage("Function call successful.");

			// get the total number of rows returned
			rs.last();
			int size = rs.getRow();
			if (size > 0)
			{
				result = new RecordDN[size];
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
				result[index++] = new RecordDN(rs);
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

	public int getIdForDomeniuPublic() throws RemoteException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call getIdForDomeniuPublic()}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);

			EMALogger.getInstance().logOtherMessage("Calling function getIdForDomeniuPublic().");
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

	public int getIdForDomeniuPrivat() throws RemoteException
	{
		Connection conn = DBInterfaceManager.getInstance().getConnection();
		int result;

		try
		{
			CallableStatement stmt = conn.prepareCall("{?= call getIdForDomeniuPrivat()}");
			stmt.registerOutParameter(1, java.sql.Types.INTEGER);

			EMALogger.getInstance().logOtherMessage("Calling function getIdForDomeniuPrivat().");
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
