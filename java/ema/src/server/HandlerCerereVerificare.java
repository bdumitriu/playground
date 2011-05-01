package server;

import data.RecordCerere;
import data.RecordTipCerere;
import data.RecordSolicitant;
import data.RecordCalitate;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Defines the operations that need to be implemented by any class that wants to handle Cereri de verificare requests.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 26, 2003
 */
public interface HandlerCerereVerificare extends Remote
{
	/**
	 * Returns true if a entry with the same cod and an as the ones in this <code>record</code>
	 * already exists in the Cerere table. All other fields in <code>record</code> than the ones mentioned above
	 * are ignored.
	 *
	 * @param record the record to look for in the database
	 * @return true if such an entry exists, false otherwise
	 */
	boolean checkCodCerere(RecordCerere record) throws RemoteException;

	/**
	 * Returns the list containing all the TipCerere's (request type) from the TipCerere table.
	 *
	 * @return the list containing all the TipCerere's (request type) from the TipCerere table
	 */
	RecordTipCerere[] getTipuriCerere() throws RemoteException;

	/**
	 * Returns the list containing all the Solicitant's (requester) from the Solicitant table.
	 *
	 * @return the list containing all the Solicitant's (requester) from the Solicitant table
	 */
	RecordSolicitant[] getSolictianti() throws RemoteException;

	/**
	 * Returns the list containing all the Calitate's (quality) from the Calitate table.
	 *
	 * @return the list containing all the Calitate's (quality) from the Calitate table
	 */
	RecordCalitate[] getCalitati() throws RemoteException;
}
