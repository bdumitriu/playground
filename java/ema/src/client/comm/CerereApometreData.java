package client.comm;

import server.EMAServer;
import server.HandlerCerereApometre;

import java.rmi.RemoteException;

import shared.EMALogger;
import shared.EMAErrorMessages;
import data.RecordFabricant;
import data.RecordDN;
import client.exception.CommunicationException;

/**
 * This class handles the business logic of the Introducere Date Apometre (watermeter data insertion) part of the
 * application.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class CerereApometreData
{
	public CerereApometreData()
	{
		remoteRef = null;
	}

	/**
	 * Returns all the Fabricant's (watermeter producers) in the Fabricant table.
	 *
	 * @return all the Fabricant's (watermeter producers) in the Fabricant table
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public RecordFabricant[] getFabricanti() throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		try
		{
			return remoteRef.getFabricanti();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Inserts a new Fabricant (watermeter producer) in the Fabricant table.
	 *
	 * @param nume the nume of the Fabricant
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public void insertFabricant(String nume) throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		try
		{
			remoteRef.insertFabricant(new RecordFabricant(-1, nume));
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}


	/**
	 * Returns all the DN's (watermeter producers) in the DN table.
	 *
	 * @return all the DN's (watermeter producers) in the DN table
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public RecordDN[] getDNs() throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		try
		{
			return remoteRef.getDNs();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Returns the appropriate id to use for domeniu public (public concern area).
	 *
	 * @return the appropriate id to use for domeniu public (public concern area)
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public int getIdForDomeniuPublic() throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		try
		{
			return remoteRef.getIdForDomeniuPublic();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Returns the appropriate id to use for domeniu privat (private concern area).
	 *
	 * @return the appropriate id to use for domeniu privat (private concern area)
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public int getIdForDomeniuPrivat() throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		try
		{
			return remoteRef.getIdForDomeniuPrivat();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Tries to obtain a reference to a remote HandlerCerereApometre object.
	 */
	private void attemptConnection()
	{
		EMAServer server = EMACommunicator.getInstance().getServerReference();

		if (server == null)
		{
			remoteRef = null;
		}
		else
		{
			try
			{
				remoteRef = server.getHandlerCerereApometre();
			}
			catch (RemoteException e)
			{
				EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
				remoteRef = null;
			}
		}
	}

	/**
	 * The remote reference to a HandlerCerereApometre object.
	 */
	private HandlerCerereApometre remoteRef;
}
