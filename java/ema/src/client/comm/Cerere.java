package client.comm;

import data.RecordApometru;
import client.exception.CommunicationException;

/**
 * This is the parent class for the two types of Cerere (request): Cerere PF (individual requeas) and Cerere PJ
 * (company request). The purpose of this class and its subclasses is to put together the data from all screens
 * gathering it and, finally, submit it to the server.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public abstract class Cerere
{
	/**
	 * Submits the entire request to the server.
	 *
	 * @param apometre the list of Apometre (watermeters) associated with this request
	 * @return 0 on success, -1 on failure
	 */
	public abstract int submitCerere(RecordApometru[] apometre) throws CommunicationException;

	public void setCodCerere(int codCerere)
	{
		this.codCerere = codCerere;
	}

	public void setAnCerere(int anCerere)
	{
		this.anCerere = anCerere;
	}

	public void setZiDD(int ziDD)
	{
		this.ziDD = ziDD;
	}

	public void setLunaDD(int lunaDD)
	{
		this.lunaDD = lunaDD;
	}

	public void setAnDD(int anDD)
	{
		this.anDD = anDD;
	}

	public void setZiDS(int ziDS)
	{
		this.ziDS = ziDS;
	}

	public void setLunaDS(int lunaDS)
	{
		this.lunaDS = lunaDS;
	}

	public void setAnDS(int anDS)
	{
		this.anDS = anDS;
	}

	public void setIdTipCerere(int idTipCerere)
	{
		this.idTipCerere = idTipCerere;
	}

	public void setIdCalitate(int idCalitate)
	{
		this.idCalitate = idCalitate;
	}

	public void setIdSolicitant(int idSolicitant)
	{
		this.idSolicitant = idSolicitant;
	}

	protected int codCerere;
	protected int anCerere;

	protected int ziDD;
	protected int lunaDD;
	protected int anDD;

	protected int ziDS;
	protected int lunaDS;
	protected int anDS;

	protected int idTipCerere;
	protected int idCalitate;
	protected int idSolicitant;
}
