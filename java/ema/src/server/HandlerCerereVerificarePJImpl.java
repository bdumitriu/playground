package server;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.io.Serializable;

/**
 * Provides an implementation of the HandlerCerereVerificarePJ interface.
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public class HandlerCerereVerificarePJImpl
	extends HandlerCerereVerificareImpl
	implements HandlerCerereVerificarePJ, Serializable
{
	public HandlerCerereVerificarePJImpl() throws RemoteException
	{}


}
