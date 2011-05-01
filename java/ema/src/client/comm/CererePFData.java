package client.comm;

import server.HandlerCerereVerificarePF;
import server.EMAServer;

import java.rmi.RemoteException;

import shared.EMALogger;
import shared.EMAErrorMessages;
import data.*;
import client.comm.EMACommunicator;
import client.exception.CommunicationException;

/**
 * This class handles the business logic of the Cerere PF (individual request) part of the application.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 23, 2003
 */
public class CererePFData
{
	public CererePFData()
	{
		remoteRef = null;
	}

	/**
	 * Returns an array containing all the PF records currently in the database for which the name contains
	 * <code>text</code>, even with other letters between those of <code>text</code>. The return value can also be
	 * null if no names match or if a communication error appears.
	 *
	 * @return the RecordPF array or null
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public RecordPF[] getListFor(String text) throws CommunicationException
	{
		if (remoteRef == null)
		{
			attemptConnection();
			if (remoteRef == null)
			{
				throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
			}
		}

		char[] temp = new char[text.length()*2+1];

		for (int i = 0; i < text.length(); i++)
		{
			temp[2*i] = '%';
			temp[2*i+1] = text.charAt(i);
		}

		temp[text.length()*2] = '%';

		try
		{
			return remoteRef.getNamesLike(new String(temp));
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Checks whether a PF with the same <code>nume</code>, <code>adresa</code> and <code>telefon</code> already
	 * exists in the PF table.
	 *
	 * @return true if it exists, false otherwise
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public boolean checkPF(String nume, String adresa, String telefon) throws CommunicationException
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
			return remoteRef.checkPF(new RecordPF(-1, nume, adresa, telefon));
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Returns all the TipCerere's (request type) in the TipCerere table.
	 *
	 * @return all the TipCerere's (request type) in the TipCerere table
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public RecordTipCerere[] getTipuriCerere() throws CommunicationException
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
			return remoteRef.getTipuriCerere();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	public RecordCalitate[] getCalitati() throws CommunicationException
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
			return remoteRef.getCalitati();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	public RecordSolicitant[] getSolicitanti() throws CommunicationException
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
			return remoteRef.getSolictianti();
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Checks whether a Cerere with the same <code>cod</code> and <code>an</code> already exists in the Cerere
	 * table.
	 *
	 * @return true if it exists, false otherwise
	 * @throws CommunicationException if errors occur during the communication with the EMA server
	 */
	public boolean checkCodCerere(int cod, int an) throws CommunicationException
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
			return remoteRef.checkCodCerere(new RecordCerere(-1, cod, an, -1, -1, -1, null, null));
		}
		catch (RemoteException e)
		{
			EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	/**
	 * Tries to obtain a reference to a remote HandlerCerereVerificarePF object.
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
				remoteRef = server.getHandlerCerereVerificarePF();
			}
			catch (RemoteException e)
			{
				EMALogger.getInstance().logDefaultRemoteExceptionMessage(e);
				remoteRef = null;
			}
		}
	}

	/**
	 * The remote reference to a HandlerCerereVerificarePF object.
	 */
	private HandlerCerereVerificarePF remoteRef;
}
