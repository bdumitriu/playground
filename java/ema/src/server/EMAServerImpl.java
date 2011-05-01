package server;

import java.rmi.server.UnicastRemoteObject;
import java.rmi.RemoteException;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 11, 2003
 */
public class EMAServerImpl extends UnicastRemoteObject implements EMAServer
{
	public EMAServerImpl() throws RemoteException
	{};

	public int getServerMajorVersion() throws RemoteException
	{
		return 0;
	}

	public int getServerMinorVersion()
	{
		return 1;
	}

	public HandlerContractPJ getHandlerContractPJ() throws RemoteException
	{
		return new HandlerContractPJImpl();
	}

	public HandlerCerereVerificarePF getHandlerCerereVerificarePF() throws RemoteException
	{
		return new HandlerCerereVerificarePFImpl();
	}

	public HandlerCerereVerificarePJ getHandlerCerereVerificarePJ() throws RemoteException
	{
		return new HandlerCerereVerificarePJImpl();
	}

	public HandlerCerereApometre getHandlerCerereApometre() throws RemoteException
	{
		return new HandlerCerereApometreImpl();
	}
}
