package client.comm;

import data.RecordApometru;
import data.RecordCerere;
import data.RecordPF;
import data.RecordCererePF;

import java.sql.Date;
import java.util.GregorianCalendar;
import java.rmi.RemoteException;

import server.EMAServer;
import server.HandlerCerereVerificarePF;
import shared.EMALogger;
import shared.EMAErrorMessages;
import client.exception.CommunicationException;

/**
 * See {@link Cerere parent class} for details.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public class CererePF extends Cerere
{
	public CererePF()
	{
		remoteRef = null;
	}

	public int submitCerere(RecordApometru[] apometre) throws CommunicationException
	{
		GregorianCalendar dd = new GregorianCalendar();
		dd.set(GregorianCalendar.YEAR, anDD);
		dd.set(GregorianCalendar.MONTH, lunaDD - 1);
		dd.set(GregorianCalendar.DAY_OF_MONTH, ziDD);

		GregorianCalendar ds = new GregorianCalendar();
		ds.set(GregorianCalendar.YEAR, anDS);
		ds.set(GregorianCalendar.MONTH, lunaDS - 1);
		ds.set(GregorianCalendar.DAY_OF_MONTH, ziDS);

		RecordCerere cerere = new RecordCerere(-1, codCerere, anCerere, idTipCerere, idCalitate, idSolicitant,
			new Date(dd.getTimeInMillis()), new Date(ds.getTimeInMillis()));

		RecordPF pf = new RecordPF(-1, nume, adresa, telefon);

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
			return remoteRef.insertCerereVerificare(cerere, pf, apometre);
		}
		catch (RemoteException e)
		{
			throw new CommunicationException(EMAErrorMessages.getInstance().getMessage("error.server.comm"));
		}
	}

	public void setNume(String nume)
	{
		this.nume = nume;
	}

	public void setAdresa(String adresa)
	{
		this.adresa = adresa;
	}

	public void setTelefon(String telefon)
	{
		this.telefon = telefon;
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

	private String nume;
	private String adresa;
	private String telefon;
}
