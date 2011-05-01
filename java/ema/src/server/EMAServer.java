package server;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Interface that defines the functionalities of the RMI server for the EMA system.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public interface EMAServer extends Remote
{
	/**
	 * Returns the server's major version number.
	 */
	public int getServerMajorVersion() throws RemoteException;

	/**
	 * Returns the server's minor version number.
	 */
	public int getServerMinorVersion() throws RemoteException;

	/**
	 * Returns a HandlerContractPJ remote object.
	 */
	public HandlerContractPJ getHandlerContractPJ() throws RemoteException;

	/**
	 * Returns a HandlerCerereVerificarePF remote object.
	 */
	public HandlerCerereVerificarePF getHandlerCerereVerificarePF() throws RemoteException;

	/**
	 * Returns a HandlerCerereVerificarePF remote object.
	 */
	public HandlerCerereVerificarePJ getHandlerCerereVerificarePJ() throws RemoteException;

	/**
	 * Returns a HandlerCerereApometre remote object.
	 */
	public HandlerCerereApometre getHandlerCerereApometre() throws RemoteException;
}
