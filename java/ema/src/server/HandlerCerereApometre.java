package server;

import data.RecordFabricant;
import data.RecordDN;
import data.RecordDomeniu;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Defines the operations that need to be implemented by any class that wants to handle Cereri de verificare requests.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public interface HandlerCerereApometre extends Remote
{
	/**
	 * Returns the list containing all the Fabricant's (watermeter producers) from the Fabricant table.
	 *
	 * @return the list containing all the Fabricant's (watermeter producers) from the Fabricant table
	 */
	public RecordFabricant[] getFabricanti() throws RemoteException;

	/**
	 * Inserts a new Fabricant (watermeter producer) in the Fabricant table.
	 *
	 * @param fabricant the record containing the data about the Fabricant
	 */
	public void insertFabricant(RecordFabricant fabricant) throws RemoteException;

	/**
	 * Returns the list containing all the DN's from the DN table.
	 *
	 * @return the list containing all the DN's from the DN table
	 */
	public RecordDN[] getDNs() throws RemoteException;

	/**
	 * Returns the id for the domeniu public (public concern area). An return value of -1 means either that an
	 * SQLException occured or that the Domeniu table did not contain the expected data.
	 *
	 * @return the id for the domeniu public (public concern area)
	 */
	public int getIdForDomeniuPublic() throws RemoteException;

	/**
	 * Returns the id for the domeniu privat (private concern area). An return value of -1 means either that an
	 * SQLException occured or that the Domeniu table did not contain the expected data.
	 *
	 * @return the id for the domeniu privat (private concern area)
	 */
	public int getIdForDomeniuPrivat() throws RemoteException;
}
