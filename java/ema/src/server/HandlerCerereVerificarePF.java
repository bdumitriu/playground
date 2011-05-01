package server;

import data.*;

import java.rmi.RemoteException;

/**
 * Defines the operations that need to be implemented by any class that wants to handle Cereri de verificare from PF
 * (individuals) requests.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public interface HandlerCerereVerificarePF extends HandlerCerereVerificare
{
	/**
	 * Inserts the <code>pf</code> into the PF table, if it's not already there, then it inserts the
	 * <code>cerere</code> into the CererePF table, associating it with the <code>pf</code> and, finally, inserts
	 * all the <code>apometre</code> in the Apometre table (if they're not already there) along with the proper
	 * entry in the CerereApometru table, properly associating each of the <code>apometre</code> with this
	 * <code>cerere</code>.
	 *
	 * @param cerere the record containing the data about the Cerere
	 * @param pf the record containing the data about the PF
	 * @param apometre the records containing data about the apometre
	 * @return 0 on success, -1 on failure
	 */
	public int insertCerereVerificare(RecordCerere cerere, RecordPF pf, RecordApometru[] apometre)
		throws RemoteException;

	/**
	 * Returns true if a entry with the same nume, adresa and telefon as the ones in this <code>record</code>
	 * already exists in the PF table. All other fields in <code>record</code> than the ones mentioned above are
	 * ignored.
	 *
	 * @param record the record to look for in the database
	 * @return true if such an entry exists, false otherwise
	 */
	public boolean checkPF(RecordPF record) throws RemoteException;

	/**
	 * This method returns an array of records from the PF table for which the name is LIKE the
	 * <code>likePattern</code> (the SQL LIKE command is used for comparison). In the pattern % stands for any
	 * sequence of characters and _ stands for any single charater. All other characters match themselves. Use the
	 * backspace ('\') character to escape the three special charaters (%, _ and \).
	 *
	 * @return an array of matching records or null if any error occurs
	 */
	public RecordPF[] getNamesLike(String likePattern) throws RemoteException;

}
