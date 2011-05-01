package geditor.engine.synch;

import geditor.repository.VersionException;
import geditor.engine.tree.GraphicDocument;
import geditor.elements.GRootGroup;

import javax.swing.*;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.ServerNotActiveException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: avancea
 * Date: Mar 2, 2004
 * Time: 2:49:41 PM
 * To change this template use File | Settings | File Templates.
 */
public class DocumentServerImpl extends UnicastRemoteObject implements DocumentServer
{
	private GraphicDocument doc;

	public DocumentServerImpl(GraphicDocument doc) throws RemoteException
	{
		this.doc = doc;
	}

	public GRootGroup getState() throws RemoteException, PermissionException
	{
		try
		{
			int result = JOptionPane.showConfirmDialog((Component) null,  "Synchronization request from \""
				+ getClientHost() + "\". " + "Accept?", "Alert", JOptionPane.YES_NO_OPTION);

			if (result != JOptionPane.YES_OPTION)
			{
				throw new PermissionException("go away!");
			}

			return doc.getRoot();
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
}
