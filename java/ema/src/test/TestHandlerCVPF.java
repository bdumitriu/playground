package test;

import server.HandlerCerereVerificarePFImpl;

import java.rmi.RemoteException;

import data.RecordPF;

/**
 *
 *
 * Author: Bogdan Dumitriu
 * Version: 0.1
 * Date: Dec 15, 2003
 */
public class TestHandlerCVPF
{
	public static void main(String[] args) throws RemoteException
	{
		HandlerCerereVerificarePFImpl handler = new HandlerCerereVerificarePFImpl();

		RecordPF[] rs = handler.getNamesLike("%P%F%");

		for (int i = 0; i < rs.length; i++)
		{
			System.out.println(rs[i].getId() + " " + rs[i].getNume() + " " + rs[i].getAdresa() + " " +
				rs[i].getTelefon());
		}
	}
}
