package geditor.engine.tree;

import geditor.elements.*;
import geditor.gui.MainFrame;
import geditor.gui.ChoiceDialog;
import geditor.gui.FindVersionDialog;
import geditor.engine.operations.*;
import geditor.engine.synch.DocumentServer;
import geditor.engine.synch.PermissionException;
import geditor.repository.RepositoryException;
import geditor.repository.VersionException;
import geditor.repository.Repository;
import geditor.tools.Datetime;

import javax.swing.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.rmi.Naming;
import java.net.MalformedURLException;

/**
 * Fill in class description here.
 * <br /><br />
 * Date: Mar 18, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class GraphicDocument
{
	private static final int NO_CONFLICT = 0;
	private static final int REAL_CONFLICT = 1;
	private static final int SOLVABLE_RIGHT_ORDER_CONFLICT = 2;
	private static final int SOLVABLE_REVERSE_ORDER_CONFLICT = 3;

	/**
	 * the current version of this document
	 */
	private int currentVersion;

	/**
	 * the root of the entire graphic tree
	 */
	private GRootGroup root;

	/**
	 * the root of the entire graphic tree as it was at the last update
	 */
	private GRootGroup lastUpdateRoot;

	/**
	 * flag indicating whether or not the previous operation was a multi select
	 */
	private boolean wasMultiselected;

	/**
	 * list containing all the selected objects at a moment in time
	 */
	private ArrayList selection;

	/**
	 * flag indicating whether or not we have already done an update from the repository
	 */
	private boolean firstupdate;

	private String rmiServer;
	private int rmiPort;

	private HashSet integratedOps;

	private int updateType = UpdateType.localWinner;

	public GraphicDocument()
	{
		root = new GRootGroup();
		selection = new ArrayList();
		currentVersion = 0;
		firstupdate = true;
		lastUpdateRoot = new GRootGroup();
		wasMultiselected = false;
	}

	public GRootGroup getRoot()
	{
		return root;
	}


	public void setCurrentVersion(int currentVersion)
	{
		this.currentVersion = currentVersion;
		MainFrame.getInstance().setVersionLabel(currentVersion);

	}

	public int getCurrentVersion()
	{
		return currentVersion;
	}

	public void addShape(GShape shape)
	{
		try
		{
			root.addShape(root.getId(), shape);
		}
		catch (TreeInconsistencyException e)
		{
			e.printStackTrace();
		}

		double minZ = shape.getParent().getMinZ();

		shape.setZ(minZ - 1);
	}

	public ArrayList getSelection()
	{
		return selection;
	}

	public void selectObject(Point2DDouble p, boolean multiselection)
	{
		try
		{
			ArrayList objList = root.getNodesByZ();
			GShape obj = null;
			boolean ok = false;
			//we needd the decresed order
			for (int i = objList.size() - 1; i >= 0; i--)
			{
				obj = ((GShape) objList.get(i));
				if (obj.insideObject(p))
				{
					select(obj.getId(), multiselection);
					ok = true;
					break;
				}
			}
			if (!ok) deselectAll();
		}
		catch (InvalidIdException e)
		{
			System.out.println(e);
		}
	}

	public void select(String id, boolean multiselection) throws InvalidIdException
	{
		GShape node = root.getShape(id);
		if (node instanceof GGroup) throw new InvalidIdException("The selection Id is a group id !!!!");

		GShape obj = node;
		if (multiselection)
		{
			wasMultiselected = true;
			MainFrame.getInstance().getObjectInspector().clear();

			//we permit selection only for the first level
			while (node.getParent() != root)
			{
				node = node.getParent();
			}
			if (!node.isSelected())
			{
				node.setSelected(true);
				selection.add(node);
			}
			else
			{
				node.setSelected(false);
				selection.remove(node);
			}

			//we get the top selected group
			// deselect all except the top group
			if (node instanceof GGroup)
				deselectAnySelectedChilds((GGroup) node, obj);
		}
		else
		{
			//clear the select list
			selection.clear();
			if (wasMultiselected)
			{
				deselectAll();
				wasMultiselected = false;
			}
			// we permit to select only one group one time
			if (node.getParent().isSelected())
			{
				// the parent group is selected
				// have to check if tha parent has brother node which are selected ,and I have to deselect them
				// Obs : we leave the parent selected,we modify only the rest
				deselectAnySelectedChilds(node.getParent(), node);
				if (obj.isSelected())
				{
					//deselect a single object
					obj.setSelected(false);
					selection.add(node.getParent());
					MainFrame.getInstance().getObjectInspector().loadValues(node.getParent().getObjectProperties());
//	                refresh(obj.getParent());
				}
				else
				{
					//select a single object
					selection.add(obj);
					obj.setSelected(true);
					MainFrame.getInstance().getObjectInspector().loadValues(obj.getObjectProperties());
					//             refresh(obj);
				}
			}
			else
			{
				//this node is a memeber of at least one group
				while ((!(node.getParent()).isSelected()) && (node.getParent() != root))
				{
					node = node.getParent();
				}
				deselectAnySelectedChilds(node.getParent(), node);
				//select this group
				node.setSelected(true);
				selection.add(node);
				MainFrame.getInstance().getObjectInspector().loadValues(node.getObjectProperties());
			}
		}
	}

	private void deselectAnySelectedChilds(GGroup node, GShape currentNode)
	{
		for (int i = 0; i < node.getNrChildren(); i++)
		{
			if (node.childAt(i).isSelected() && (currentNode != node.childAt(i)))
				deselectSubtree(node.childAt(i));
		}
	}

	private void deselectSubtree(GShape node)
	{
		if (node instanceof GGroup)
		{
			node.setSelected(false);
			selection.remove(node);
			for (int i = 0; i < ((GGroup) node).getNrChildren(); i++)
			{
				deselectSubtree(((GGroup) node).childAt(i));
			}
		}
		else
		{
			// this is am object node
			node.setSelected(false);
			selection.remove(node);
		}
	}

	public void deselectAll()
	{
		deselectSubtree(root);
		selection.clear();
		MainFrame.getInstance().getObjectInspector().clear();
	}


	public void setRmiServer(String rmiServer)
	{
		this.rmiServer = rmiServer;
	}

	public void setRmiPort(int rmiPort)
	{
		this.rmiPort = rmiPort;
	}

	public void commit()
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			if (remoteRef.getCurrentVersion() != getCurrentVersion())
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository version has changed.\n" +
					"Please update before committing.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else if (remoteRef.commit(root, getCurrentVersion()))
			{
				setCurrentVersion(getCurrentVersion() + 1);
				lastUpdateRoot = (GRootGroup) root.clone();
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Commit successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository version has changed.\n" +
					"Please update before committing.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository was unable to commit your data.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	public void update()
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);
			int repVersion = remoteRef.getCurrentVersion();

			if (repVersion == getCurrentVersion())
			{
				if (!firstupdate)
				{
					JOptionPane.showMessageDialog(MainFrame.getInstance(), "You are already up to date.", "Info",
						JOptionPane.INFORMATION_MESSAGE);
				}

				firstupdate = false;
				return;
			}

			GRootGroup remoteState = remoteRef.getState(repVersion);
			internalUpdate(remoteState);
			setCurrentVersion(repVersion);

			MainFrame.getInstance().getWorkArea().repaint();

			if (!firstupdate)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Update successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}

			firstupdate = false;

			deselectAll();
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository was unable to send the information.",
				"Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
	}

	/**
	 * Synchronizes with host denoted by <code>ip</code>.
	 *
	 * @param asMaster if true, in case of conflict, the changes that this host made will be kept, if false, the
	 *	changes that the remote host made will be kept
	 */
	public void synchronize(String ip, boolean asMaster)
	{
		String url = "rmi://" + ip + ":1099/docserver";

		try
		{
			DocumentServer docserRef = (DocumentServer) Naming.lookup(url);
			GRootGroup remoteState = docserRef.getState();


			int oldut = updateType;

			if (updateType != UpdateType.choice)
			{
				if (asMaster) updateType = UpdateType.localWinner;
				else    updateType = UpdateType.remoteWinner;
			}
			internalSynch(remoteState);

			updateType = oldut;

			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Update successful.", "Info",
				JOptionPane.INFORMATION_MESSAGE);

			deselectAll();
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();

		}
		catch (PermissionException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Permission denied.",
				"Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
	}


	private void merge(GRootGroup root, GShape shape)
	{
		if (root.getShape(shape.getId()) == null)
		{
			GShape x = (GShape) shape.clone();
			if (x instanceof GGroup) ((GGroup) x).deleteAllChildren();
			try
			{
				root.addShape(shape.getParent().getId(), x);
			}
			catch (TreeInconsistencyException e)
			{
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}
			x.setParent2(x.getParent());
			x.setParent(null);
		}
		else
		if (!shape.getId().equals(root.getId())) //is not root
		{
			GShape lshape = root.getShape(shape.getId());
			GGroup rparent = (GGroup) root.getShape(shape.getParent().getId());
			if (rparent.findChild(lshape.getId()) == null) rparent.addChild(lshape);
			lshape.setParent2(rparent);
		}

        if (shape instanceof GGroup)
        {
	        GGroup gshape = (GGroup) shape;
	        for (int i = 0; i < gshape.getNrChildren(); i++)
		            merge(root, gshape.childAt(i));
        }


	}

	// shape - current shape
	// orgg - the original tree (last update root)
	private void reduceDAG(GShape shape, GRootGroup orgg, boolean keeplocalversion)
	{

		if (shape.isVisited()) return;
		shape.setVisited(true);

		if (shape instanceof GGroup)
		{
			GGroup gshape = (GGroup) shape;
			boolean bb = true;
			while (bb)
			{
				bb = false;
				for (int i = 0; i < gshape.getNrChildren(); i++)
						if (!gshape.childAt(i).isVisited())
						{
							reduceDAG(gshape.childAt(i), orgg, keeplocalversion);
							bb = true;
							break;
						}

			}
		}

		if (shape.getParent() == null && shape.getParent2() == null) return;

		// newly created shape

        if (orgg.getShape(shape.getId()) == null)
        {
	        if (shape.getParent() == null)
	        {
		        shape.setParent(shape.getParent2());
		        shape.setParent2(null);
	        }
	        else shape.setParent2(null);
	        return;
        }

		if (shape.getParent() == shape.getParent2())
		{
			shape.setParent2(null);
			return;
		}

		//orginal ParentId
		String orgPid = orgg.getShape(shape.getId()).getParent().getId();

		//shape deleted locally
		if (shape.getParent() == null)
		{
			// the remote user did not change anything
			if (shape.getParent2().getId().equals(orgPid) ||
				keeplocalversion)
			{
				shape.getParent2().deleteChild(shape.getId());
				shape.setParent2(null);
			}
			else
			{
				shape.setParent(shape.getParent2());
				shape.setParent2(null);
			}
			return;
		}

		//shape deleted remotelly
		if (shape.getParent2() == null)
		{
			if (shape.getParent().getId().equals(orgPid) ||
				!keeplocalversion)
			{
				shape.getParent().deleteChild(shape.getId());
				shape.setParent(null);
			}
			return;
		}

		// the shape exists in both trees

		if (shape.getParent().getId().equals(orgPid))
		{
			shape.getParent().deleteChild(shape.getId());
			shape.setParent(shape.getParent2());
			shape.setParent2(null);
			return;
		}

		if (shape.getParent2().getId().equals(orgPid))
		{
			shape.getParent2().deleteChild(shape.getId());
			shape.setParent2(null);
			return;
		}

		if (keeplocalversion)
		{
			shape.getParent2().deleteChild(shape.getId());
			shape.setParent2(null);
		}
		else
		{
			shape.getParent().deleteChild(shape.getId());
			shape.setParent(shape.getParent2());
			shape.setParent2(null);
		}

	}

	private void internalSynch(GRootGroup remoteG)
	{

		if (updateType == UpdateType.choice)
		{
			GRootGroup orgroot = (GRootGroup) root.clone();
			//orginal last update root
			GRootGroup orglur = (GRootGroup) lastUpdateRoot.clone();

			updateType = UpdateType.localWinner;
			internalSynch(remoteG);

			GRootGroup lwroot = root;
			GRootGroup lwlur = lastUpdateRoot;

			root = orgroot;
			lastUpdateRoot = orglur;

			updateType = UpdateType.remoteWinner;
			internalSynch(remoteG);

			GRootGroup rwroot = root;
			GRootGroup rwlur = lastUpdateRoot;

			ChoiceDialog cd = new ChoiceDialog(MainFrame.getInstance(), lwroot, rwroot);
			if (!lwroot.equals(rwroot))
				cd.show();
			else
				cd.setChoice(1);
			
			if (cd.getChoice() == 1)
			{
				root = lwroot;
				lastUpdateRoot = lwlur;
			}
			else
			{
				root = rwroot;
				lastUpdateRoot = rwlur;
			}
			updateType = UpdateType.choice;
			return;
		}

		//updating leafs
		ArrayList leafs = root.getLeafs();
		for (int i = 0; i < leafs.size(); i++)
		{
			String id = (String) leafs.get(i);

			GShape sl = root.getShape(id); //local
			GShape sr = remoteG.getShape(id); //remote
			GShape so = lastUpdateRoot.getShape(id); // orginal

			if (so == null) continue; // newly created
			if (sl != null && sr != null)
				sl.merge(sr, so, updateType == UpdateType.localWinner);
		}

		merge(root, remoteG);

		root.clearVisitedFlag();
		reduceDAG(root, lastUpdateRoot, updateType == UpdateType.localWinner );

		root.removeEmptyGroups();
	}


	private void internalUpdate(GRootGroup remoteG)
	{

		if (updateType == UpdateType.choice)
		{
			GRootGroup orgroot = (GRootGroup) root.clone();
			//orginal last update root
			GRootGroup orglur = (GRootGroup) lastUpdateRoot.clone();

			updateType = UpdateType.localWinner;
			internalUpdate(remoteG);

			GRootGroup lwroot = root;
			GRootGroup lwlur = lastUpdateRoot;

			root = orgroot;
			lastUpdateRoot = orglur;

			updateType = UpdateType.remoteWinner;
			internalUpdate(remoteG);

			GRootGroup rwroot = root;
			GRootGroup rwlur = lastUpdateRoot;

			ChoiceDialog cd = new ChoiceDialog(MainFrame.getInstance(), lwroot, rwroot);
			if (!lwroot.equals(rwroot))
				cd.show();
			else
				cd.setChoice(1);

			if (cd.getChoice() == 1)
			{
				root = lwroot;
				lastUpdateRoot = lwlur;
			}
			else
			{
				root = rwroot;
				lastUpdateRoot = rwlur;
			}
			updateType = UpdateType.choice;
			return;
		}

		//updating leafs
		ArrayList leafs = root.getLeafs();
		for (int i = 0; i < leafs.size(); i++)
		{
			String id = (String) leafs.get(i);

			GShape sl = root.getShape(id); //local
			GShape sr = remoteG.getShape(id); //remote
			GShape so = lastUpdateRoot.getShape(id); // orginal

			if (so == null) continue; // newly created
			if (sl != null && sr != null)
				sl.merge(sr, so, updateType == UpdateType.localWinner);
		}

		merge(root, remoteG);

		root.clearVisitedFlag();
		reduceDAG(root, lastUpdateRoot, updateType == UpdateType.localWinner );

		root.removeEmptyGroups();
		lastUpdateRoot = (GRootGroup) remoteG.clone();

	}




	public void setUpdateType(int updateType)
	{
		this.updateType = updateType;
	}

	public String getRmiServer()
	{
		return rmiServer;
	}

	public int getRmiPort()
	{
		return rmiPort;
	}

	/**
	 * The method returns the ArrayList that contains all <code>ops</code> modified so that they include the effect
	 * of <code>op</code>.
	 */


	public void checkOutVersion(int checkOutVersion)
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			root = new GRootGroup();
			selection = new ArrayList();
			firstupdate = false;
			lastUpdateRoot = new GRootGroup();

			wasMultiselected = false;

			root = remoteRef.getState(checkOutVersion);
			lastUpdateRoot = (GRootGroup) root.clone();
			setCurrentVersion(checkOutVersion);

			MainFrame.getInstance().getWorkArea().repaint();
			deselectAll();
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.", "Error",
				JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. Check server and port.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The version sent to the repository seems to be " +
				"corrupt. Please restart program.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RepositoryException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository was unable to send the information.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	public void findVersionByDate()
	{
		FindVersionDialog fvDialog = new FindVersionDialog();

		fvDialog.show();

		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			Datetime timestamp = new Datetime();
			timestamp.setYear(fvDialog.getYear());
			timestamp.setMonth(fvDialog.getMonth());
			timestamp.setDay(fvDialog.getDay());
			timestamp.setHour(fvDialog.getHour());
			timestamp.setMinute(fvDialog.getMinute());
			timestamp.setSecond((byte) 0);
			timestamp.setMilli((short) 0);

			int ver = remoteRef.getVersionByTimestamp(timestamp);

			if (ver == -1)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "All versions on " +
					"repository was committed before the date you specified.", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}
			else
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "The version number " +
					"closest to date you specified is " + ver + ".", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}
		}
		catch (NotBoundException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (MalformedURLException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Couldn't contact repository.",
				"Error", JOptionPane.ERROR_MESSAGE);
		}
		catch (RemoteException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "RMI error in comunication. " +
				"Check server and port.", "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
}