package server;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;
import java.io.Serializable;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public class HandlerContractPJImpl
	extends UnicastRemoteObject
	implements HandlerContractPJ, Serializable
{
	public HandlerContractPJImpl() throws RemoteException
	{}
}
