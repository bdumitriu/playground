package comm.dusync;

import comm.repository.VersionException;

import javax.swing.*;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.ServerNotActiveException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.awt.*;

import core.Document;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 2, 2004
 * Time: 2:49:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class DocumentServerImpl extends  UnicastRemoteObject
                                implements DocumentServer
{
	/**
	 * Creates a new document server implementation instance associated with <code>doc</code>.
	 */
	public DocumentServerImpl(Document doc) throws RemoteException
	{
		this.doc = doc;
	}

	/**
	 * {@inheritDoc}
	 */
	public ArrayList getOperations(int version) throws RemoteException, PermissionException, VersionException
	{
		if (version != doc.getCurrentVersion())
		{
			throw new VersionException("Invalid version");
		}

		try
		{
			int result = JOptionPane.showConfirmDialog((Component) null,  "Synchronization request from \""
				+ getClientHost() + "\". " + "Accept?", "Alert", JOptionPane.YES_NO_OPTION);

			if (result != JOptionPane.YES_OPTION)
			{
				throw new PermissionException("Permission denied.");
			}

			return doc.getSynchOperations();
		}
		catch (ServerNotActiveException e)
		{
			e.printStackTrace();
		}
		catch (HeadlessException e)
		{
			e.printStackTrace();
		}

		return null;
	}

	/**
	 * the document this server serves
	 */
	private Document doc;
}