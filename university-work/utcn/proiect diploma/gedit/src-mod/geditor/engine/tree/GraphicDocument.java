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
	 * the history of locally executed operations
	 */
	private ArrayList history;

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
		history = new ArrayList();
		wasMultiselected = false;
	}

	public GRootGroup getRoot()
	{
		return root;
	}

	public ArrayList getHistory()
	{
		return history;
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

	public void addToHistory(Operation o)
	{
		boolean dontadd = false;

		if (o instanceof TranslationOperation)
		{
			TranslationOperation to = (TranslationOperation) o;
			if (Math.abs(to.getDx()) < 1 && Math.abs(to.getDy()) < 1) return;
		}

		if (o instanceof DeleteOperation)
		{
			DeleteOperation od = (DeleteOperation) o;
			for (int i = 0; i < history.size(); i++)
				if (history.get(i) instanceof DeleteOperation)
				{
					DeleteOperation x = (DeleteOperation) history.get(i);
					if (od.getShape().existsId(x.getParentId()))
					{
						((Operation) x.invert().get(0)).applyTo(od.getShapeGroup());
						history.remove(i);
						i--;
					}
				}
				else if (od.getShape().existsId(((Operation) history.get(i)).getShapeId()))
				{
					Operation oh = (Operation) history.get(i);

					if (oh instanceof CreateOperation || oh instanceof GroupOperation)
						continue;

					history.remove(i);
					i--;
				}
		}


		if (!dontadd)
			history.add(o.clone());

		System.out.println("---BEGIN LOG---");
		for (int i = 0; i < history.size(); i++)
			System.out.println(history.get(i));
		System.out.println("---END LOG---");

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

			compress(getCommitLog(), lastUpdateRoot);
			if (remoteRef.getCurrentVersion() != getCurrentVersion())
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "The repository version has changed.\n" +
					"Please update before committing.", "Info", JOptionPane.INFORMATION_MESSAGE);
			}
			else if (remoteRef.commit(getCommitLog(), getCurrentVersion()))
			{
				setCurrentVersion(getCurrentVersion() + 1);
				lastUpdateRoot = (GRootGroup) root.clone();
				emptyLog();
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Commit successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);

				//version.setText(new Integer(doc.getCurrentVersion()).toString());
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

		compress(getCommitLog(), lastUpdateRoot);		

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

			for (int i = getCurrentVersion(); i < repVersion; i++)
			{
				ArrayList remoteLog = remoteRef.getOperations(i, i + 1);
				internalUpdate(remoteLog);
				setCurrentVersion(getCurrentVersion() + 1);
			}

			MainFrame.getInstance().getWorkArea().repaint();

			if (!firstupdate)
			{
				JOptionPane.showMessageDialog(MainFrame.getInstance(), "Update successful.", "Info",
					JOptionPane.INFORMATION_MESSAGE);
			}

			//version.setText(new Integer(doc.getCurrentVersion()).toString());

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
			ArrayList ops = docserRef.getOperations(getCurrentVersion());

			internalSynch(ops, asMaster);

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
		catch (VersionException e)
		{
			JOptionPane.showMessageDialog(MainFrame.getInstance(), "Not the same version.",
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

	public ArrayList getSynchHistory()
	{
		compress(getCommitLog(), lastUpdateRoot);

		if (integratedOps == null)
		{
			integratedOps = new HashSet();
		}


		for (int i = 0; i < history.size(); i++)
		{
			Operation op = (Operation) history.get(i);
			op.setSynched(true);
			integratedOps.add(op.getId());
		}

		return history;
	}

	private ArrayList getCommitLog()
	{
		return history;
	}

	private void emptyLog()
	{
		history.clear();
	}

	private void internalUpdate(ArrayList remoteLog)
	{
		ArrayList orgRemoteLog = (ArrayList) remoteLog.clone();

		compress(getCommitLog(), lastUpdateRoot);

		if (integratedOps != null)
		{
			for (int i = 0; i < remoteLog.size(); i++)
			{
				if (integratedOps.contains(((Operation) remoteLog.get(i)).getId()))
				{

					String rid = ((Operation) remoteLog.get(i)).getId();
					for (int j = 0; j < history.size(); j++)
					{
						Operation o = (Operation) history.get(j);
						if (o.getId().equals(rid)) o.setFromRepository(true);
					}
															
					remoteLog.remove(i);
					i--;
				}
			}
		}

		HashSet tempHash = new HashSet();
		for (int i = 0; i < remoteLog.size(); i++)
		{
			ArrayList intOps = ((Operation) (remoteLog.get(i))).getIntegratedOps();
			if (intOps != null)
			{
				tempHash.addAll(intOps);
			}
		}

		for (int i = 0; i < history.size(); i++)
		{
			if (tempHash.contains(((Operation) history.get(i)).getId()))
			{
				history.remove(i);
				i--;
			}
		}

		tempHash = null;

		for (int i = 0; i < remoteLog.size(); i++)
			((Operation)remoteLog.get(i)).setFromRepository(true);

		System.out.println("<remotelog>");
		for (int i = 0; i < remoteLog.size(); i++)
				System.out.println(remoteLog.get(i));
		System.out.println("</remotelog>");

		history = merge((ArrayList) remoteLog.clone(), history);

		root = (GRootGroup) lastUpdateRoot.clone();
		GRootGroup compressroot = (GRootGroup) lastUpdateRoot.clone();



		System.out.println("<remotelog2>");
		for (int i = 0; i < remoteLog.size(); i++)
				System.out.println(remoteLog.get(i));
		System.out.println("</remotelog2>");



		for (int i = 0; i < orgRemoteLog.size(); i++)
		{
			((Operation) orgRemoteLog.get(i)).applyTo(compressroot);
//			((Operation) remoteLog.get(i)).applyTo(root);
		}


		System.out.println("<history>");
		for (int i = 0; i < history.size(); i++)
				System.out.println(history.get(i));
		System.out.println("</history>");


		for (int i = 0; i < history.size(); i++)
		{
			((Operation) history.get(i)).applyTo(root);
		}


		System.out.println("<remotelog3>");
		for (int i = 0; i < remoteLog.size(); i++)
				System.out.println(remoteLog.get(i));
		System.out.println("</remotelog3>");


		for (int i =  0; i < orgRemoteLog.size(); i++)
			history.addAll(0, ((Operation)orgRemoteLog.get(i)).invert());

		compress(history, compressroot);

		for (int i = 0; i < history.size(); i++)
			((Operation)history.get(i)).setFromRepository(false);


		lastUpdateRoot = (GRootGroup) compressroot.clone();
	}



	public void internalSynch(ArrayList remoteLog, boolean asMaster)
	{
		compress(getCommitLog(), lastUpdateRoot);


		if (integratedOps != null)
		{
			for (int i = 0; i < remoteLog.size(); i++)
			{
				if (integratedOps.contains(((Operation) remoteLog.get(i)).getId()))
				{

					String rid = ((Operation) remoteLog.get(i)).getId();
					for (int j = 0; j < history.size(); j++)
					{
						Operation o = (Operation) history.get(j);
						if (o.getId().equals(rid)) o.setFromRepository(true);
					}
					
					remoteLog.remove(i);
					i--;
				}
			}
		}
		else
		{
			integratedOps = new HashSet();
		}

		HashSet tempHash = new HashSet();
		for (int i = 0; i < remoteLog.size(); i++)
		{
			ArrayList intOps = ((Operation) (remoteLog.get(i))).getIntegratedOps();
			if (intOps != null)
			{
				tempHash.addAll(intOps);
			}
		}

		for (int i = 0; i < history.size(); i++)
		{
			if (tempHash.contains(((Operation) history.get(i)).getId()))
			{
				history.remove(i);
				i--;
			}
		}

		tempHash = null;

		for (int i = 0; i < remoteLog.size(); i++)
		{
			Operation op = (Operation) remoteLog.get(i);
			op.setFromRepository(true);
		}

//		System.out.println("<remotelog>");
//		for (int i = 0; i < remoteLog.size(); i++)
//				System.out.println(remoteLog.get(i));
//		System.out.println("</remotelog>");

		int oldUpdateType = updateType;

		if (asMaster)
		{
			updateType = UpdateType.localWinner;
		}
		else
		{
			updateType = UpdateType.remoteWinner;
		}

		history = merge((ArrayList) remoteLog.clone(), history);

		updateType = oldUpdateType;

		root = (GRootGroup) lastUpdateRoot.clone();
		GRootGroup compressroot = (GRootGroup) lastUpdateRoot.clone();

//		System.out.println("<history>");
//		for (int i = 0; i < history.size(); i++)
//				System.out.println(history.get(i));
//		System.out.println("</history>");

		for (int i = 0; i < history.size(); i++)
		{
			((Operation) history.get(i)).applyTo(root);
		}

//		System.out.println("<remotelog3>");
//		for (int i = 0; i < remoteLog.size(); i++)
//			System.out.println(remoteLog.get(i));
//		System.out.println("</remotelog3>");

		compress(history, compressroot);

		for (int i = 0; i < history.size(); i++)
		{

			Operation op = (Operation) history.get(i);
			if (op.isFromRepository()) integratedOps.add(op.getId());
			
			((Operation) history.get(i)).setFromRepository(false);
		}



	}

	public ArrayList merge(ArrayList remoteLog, ArrayList localLog)
	{
		ArrayList resultLog = localLog;

		GRootGroup RCT = (GRootGroup) lastUpdateRoot.clone();

		// this array is used to store the positions of those remote operations which are found to be dependent
		// on remote operations which haven't been executed because they were in real conflict with one or more
		// local operations and the decision was to keep the local operation(s)
		boolean dependentOf[] = new boolean[remoteLog.size()];
		for (int i = 0; i < dependentOf.length; i++)
		{
			dependentOf[i] = false;
		}

		for (int i = 0; i < remoteLog.size(); i++)
		{
			Operation remoteOp = (Operation) remoteLog.get(i);

			if (dependentOf[i])
			{
				remoteOp.applyTo(RCT);
				continue;
			}

			ArrayList tempResultLog = new ArrayList();
			boolean remoteOpRemoved = merge(remoteOp, resultLog, RCT, tempResultLog);
			resultLog = tempResultLog;

			if (remoteOpRemoved)
			{
				ArrayList temp = getDependentOps(i, remoteLog);
				for (int j = 0; j < temp.size(); j++)
				{
					dependentOf[((Integer) temp.get(j)).intValue()] = true;
				}
			}

			remoteOp.applyTo(RCT);
		}

		return resultLog;
	}

	/**
	 * @param remoteOp the remote operation to integrate into the local log
	 * @param localLog the local log
	 * @param remoteContext the context in which the remoteOp has been executed (remotely)
	 * @param resultLog the resulting local log (after the integration of <code>remoteOp</code>)
	 * @return true if the remote operation was in a real conflict with one or more of the local operations and the
	 *	decision was to discard the remote operation and false otherwise
	 */
	public boolean merge(Operation remoteOp, ArrayList localLog, GRootGroup remoteContext, ArrayList resultLog)
	{
		ArrayList realConflictOps = new ArrayList();
		ArrayList reverseOrderOps = new ArrayList();
		ArrayList rightOrderOps = new ArrayList();
		ArrayList independentOps = new ArrayList();

		// this array is used to store the positions of those operations which are found to be dependent on
		// operations found to be in a conflict with others
		byte dependentOf[] = new byte[localLog.size()];
		for (int i = 0; i < dependentOf.length; i++)
		{
			dependentOf[i] = NO_CONFLICT;
		}

		GRootGroup LCT = (GRootGroup) lastUpdateRoot.clone();
		GRootGroup RCT = remoteContext;

		for (int i = 0; i < localLog.size(); i++)
		{
			Operation localOp = (Operation) localLog.get(i);

			if (dependentOf[i] == REAL_CONFLICT)
			{
				realConflictOps.add(new Integer(i));
				localOp.applyTo(LCT);
				continue;
			}
			else if (dependentOf[i] == SOLVABLE_RIGHT_ORDER_CONFLICT)
			{
				rightOrderOps.add(new Integer(i));
				localOp.applyTo(LCT);
				continue;
			}
			else if (dependentOf[i] == SOLVABLE_REVERSE_ORDER_CONFLICT)
			{
				reverseOrderOps.add(new Integer(i));
				localOp.applyTo(LCT);
				continue;
			}

			int conflictResult = semanticConflict(remoteOp, localOp, RCT, LCT);

			switch (conflictResult)
			{
				case SOLVABLE_RIGHT_ORDER_CONFLICT:				
				case NO_CONFLICT:
				{
					independentOps.add(new Integer(i));
					break;
				}
				case REAL_CONFLICT:
				{
					realConflictOps.add(new Integer(i));

					ArrayList temp = getDependentOps(i, localLog);
					for (int j = 0; j < temp.size(); j++)
					{
						dependentOf[((Integer) temp.get(j)).intValue()] = REAL_CONFLICT;
					}

					break;
				}
				/*case SOLVABLE_RIGHT_ORDER_CONFLICT:
				{
					rightOrderOps.add(new Integer(i));

					ArrayList temp = getDependentOps(i, localLog);
					for (int j = 0; j < temp.size(); j++)
					{
						dependentOf[((Integer) temp.get(j)).intValue()] = SOLVABLE_RIGHT_ORDER_CONFLICT;
					}

					break;
				}*/
				case SOLVABLE_REVERSE_ORDER_CONFLICT:
				{
					reverseOrderOps.add(new Integer(i));

					ArrayList temp = getDependentOps(i, localLog);
					for (int j = 0; j < temp.size(); j++)
					{
						dependentOf[((Integer) temp.get(j)).intValue()] = SOLVABLE_REVERSE_ORDER_CONFLICT;
					}

					break;
				}
			}

			localOp.applyTo(LCT);
		}

		int choice;
		if (realConflictOps.isEmpty())
		{
			choice = 1;
		}
		else
		{
			choice = choice(independentOps, rightOrderOps, reverseOrderOps, realConflictOps, remoteOp,
				localLog);
		}

		if (choice == 1)
		{
			for (int i = 0; i < independentOps.size(); i++)
			{
				resultLog.add(localLog.get(((Integer) independentOps.get(i)).intValue()));
			}

			for (int i = 0; i < rightOrderOps.size(); i++)
			{
				resultLog.add(localLog.get(((Integer) rightOrderOps.get(i)).intValue()));
			}

			resultLog.add(remoteOp);

			for (int i = 0; i < reverseOrderOps.size(); i++)
			{
				resultLog.add(localLog.get(((Integer) reverseOrderOps.get(i)).intValue()));
			}
		}
		else
		{
			// don't change anything if the remote operation was in a real conflict with one or more local
			// operations and the decision was to keep the local operation(s)
			resultLog.addAll(localLog);
			return true;
		}

		return false;
	}

	/**
	 * Returns all operations from <code>log</code>, starting from operation <code>index</code> + 1, which are
	 * directly or indirectly dependent on operation <code>log</code>(<code>index</code>). Dependent operations
	 * are those which would not be able to (properly) execute if the original operation wouldn't.
	 *
	 * @param index the position in <code>log</code> at which the target operation is
	 * @param log the log of Operation's
	 * @return an ArrayList of Integer objects indicating the positions of dependent operations
	 */
	private ArrayList getDependentOps(int index, ArrayList log)
	{
		Operation localOp = (Operation) log.get(index);

		if (!(localOp instanceof GroupOperation || localOp instanceof UngroupOperation))
		{
			return new ArrayList();
		}

		ArrayList result = new ArrayList();

		ArrayList depOps = new ArrayList();
		depOps.add(new Integer(index));
		while (!depOps.isEmpty())
		{
			int idx = ((Integer) depOps.remove(depOps.size() - 1)).intValue();
			Operation currentOp = (Operation) log.get(idx);

			for (int i = idx + 1; i < log.size(); i++)
			{
				Operation logOp = (Operation) log.get(i);

/*				if (currentOp instanceof GroupOperation)
				{
					if (logOp instanceof GroupOperation)
					{
						ArrayList children = ((GroupOperation) logOp).getChildren();
						for (int j = 0; j < children.size(); j++)
						{
							GShape curChild = (GShape) children.get(j);
							if (curChild.getId().equals(currentOp.getShapeId()))
							{
								if (!result.contains(new Integer(i)))
								{
									result.add(new Integer(i));
									depOps.add(new Integer(i));
								}
								j = children.size();
							}
						}
					}
					else if (logOp.getShapeId().equals(currentOp.getShapeId()))
					{
						if (!result.contains(new Integer(i)))
						{
							result.add(new Integer(i));
							depOps.add(new Integer(i));
						}
					}
				}
				else*/ if (currentOp instanceof UngroupOperation)
				{
					if (logOp instanceof GroupOperation)
					{
						ArrayList groupChildren = ((GroupOperation) logOp).getChildren();
						ArrayList ungroupChildren = ((UngroupOperation) currentOp).getChildren();

						for (int j = 0; j < groupChildren.size(); j++)
						{
							for (int k = 0; k < ungroupChildren.size(); k++)
							{
								GShape groupChild = (GShape) groupChildren.get(j);
								GShape ungroupChild = (GShape) ungroupChildren.get(k);
								if (groupChild.getId().equals(ungroupChild.getId()))
								{
									if (!result.contains(new Integer(i)))
									{
										result.add(new Integer(i));
										depOps.add(new Integer(i));
									}
									j = groupChildren.size();
									k = ungroupChildren.size();
								}
							}
						}
					}
				}
			}
		}

		return result;
	}

	/**
	 * This method deals with making the decision in case of a conflict between the <code>remoteOp</code> and the
	 * list of operations from <code>localLog</code> whose positions are stored as Integer's in the list of
	 * <code>realConflictOps</code>.
	 *
	 * @param independentOps the list of Integer's identifying the positions of operations from
	 * <code>localLog</code> which are independent of <code>remoteOp</code>
	 * @param rightOrderOps the list of Integer's identifying the positions of operations from <code>localLog</code>
	 * which are in solvable conflict with <code>remoteOp</code> and should be executed <b>after</b>
	 * <code>remoteOp</code>
	 * @param reverseOrderOps the list of Integer's identifying the positions of operations from
	 * <code>localLog</code> which are in solvable conflict with <code>remoteOp</code> and should be executed
	 * <b>before</b> <code>remoteOp</code>
	 * @param realConflictOps the list of Integer's identifying the positions of operations from
	 * <code>localLog</code> which are in real conflict with <code>remoteOp</code>
	 * @param remoteOp the remote operation which is being integrated into the local log
	 * @param localLog the local log of Operation's
	 * @return 0 if the local operations are to be kept and 1 if the remote operation is to be kept
	 */
	private int choice(ArrayList independentOps, ArrayList rightOrderOps, ArrayList reverseOrderOps,
		ArrayList realConflictOps, Operation remoteOp, ArrayList localLog)
	{
		if  (updateType == UpdateType.localWinner) return 0;
		if  (updateType == UpdateType.remoteWinner) return 1;

		//updateType == UpdateType.choice
		ArrayList localWinnerOpList =  (ArrayList) localLog.clone();

		ArrayList remoteWinnerOpList = new ArrayList();

		for (int i = 0; i < independentOps.size(); i++)
		{
		    	Integer x = (Integer) independentOps.get(i);
				remoteWinnerOpList.add(localLog.get(x.intValue()));
		}

		for (int i = 0; i < rightOrderOps.size(); i++)
		{
		    	Integer x = (Integer) rightOrderOps.get(i);
				remoteWinnerOpList.add(localLog.get(x.intValue()));
		}

		remoteWinnerOpList.add(remoteOp);

		remoteWinnerOpList.addAll(reverseOrderOps);
		for (int i = 0; i < reverseOrderOps.size(); i++)
		{
		    	Integer x = (Integer) reverseOrderOps.get(i);
				remoteWinnerOpList.add(localLog.get(x.intValue()));
		}

		GRootGroup localWinnerRoot = (GRootGroup) lastUpdateRoot.clone();
		for (int i = 0; i < localWinnerOpList.size(); i++)
		{
			Operation op = (Operation) localWinnerOpList.get(i);
			op.applyTo(localWinnerRoot);
		}

		GRootGroup remoteWinnerRoot = (GRootGroup) lastUpdateRoot.clone();
		for (int i = 0; i < remoteWinnerOpList.size(); i++)
		{
			Operation op = (Operation) remoteWinnerOpList.get(i);
			op.applyTo(remoteWinnerRoot);
		}

		localWinnerRoot.clearSelection();
		remoteWinnerRoot.clearSelection();
		localWinnerRoot.selectAllGroups();
		remoteWinnerRoot.selectAllGroups();

		localWinnerRoot.setSelected(false);
		remoteWinnerRoot.setSelected(false);

		ChoiceDialog cd = new ChoiceDialog(MainFrame.getInstance(), localWinnerRoot, remoteWinnerRoot);
		cd.show();
		if (cd.getChoice() == 1) return 0;
		if (cd.getChoice() == 2) return 1;

		return 0;
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

	private boolean isParent(String pid, String sid, GRootGroup g)
	{
		GShape x = g.getShape(sid);
		if (x == null) return false;
		x = x.getParent();
		while (x != null)
		{
			if (x.getId().equals(pid))
			{
				return true;
			}
			x = x.getParent();
		}
		return false;
	}

	/**
	 * @param op1
	 * @param op2
	 * @param remoteContext
	 * @param localContext
	 * @return one of NO_CONFLICT, REAL_CONFLICT, SOLVABLE_RIGHT_ORDER_CONFLICT, SOLVABLE_REVERSE_ORDER_CONFLICT
	 */
	private int semanticConflict(Operation op1, Operation op2, GRootGroup remoteContext,
		GRootGroup localContext)
	{

		if (op1.isFromRepository() && op2.isFromRepository())
												return NO_CONFLICT;
		if (op1 instanceof CreateOperation || op2 instanceof CreateOperation || op2 instanceof NopOperation ||
		        op1 instanceof NopOperation)
			return NO_CONFLICT;
		Operation x = op1;
		Operation y = op2;

		GRootGroup cx = remoteContext;
		GRootGroup cy = localContext;

		for (int i = 0; i < 2; i++)
		{
			if (x.getShapeId().equals(y.getShapeId()))
			{
				if (x instanceof DeleteOperation && y instanceof DeleteOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof UngroupOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof SetBgColorOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof SetFgColorOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof TranslationOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof ScaleOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof SetTextOperation) return REAL_CONFLICT;

				if (x instanceof UngroupOperation && y instanceof UngroupOperation) return REAL_CONFLICT;
				if (x instanceof SetFgColorOperation && y instanceof SetFgColorOperation) return REAL_CONFLICT;
				if (x instanceof SetBgColorOperation && y instanceof SetBgColorOperation) return REAL_CONFLICT;
				if (x instanceof SetTextOperation && y instanceof SetTextOperation) return REAL_CONFLICT;
				if (x instanceof ScaleOperation && y instanceof ScaleOperation) return REAL_CONFLICT;
				if (x instanceof TranslationOperation && y instanceof TranslationOperation) return REAL_CONFLICT;


				if (x instanceof SetZOperation && y instanceof SetZOperation) return REAL_CONFLICT;
				if (x instanceof DeleteOperation && y instanceof SetZOperation) return REAL_CONFLICT;

			}

			if (x instanceof DeleteOperation &&
			        isParent(x.getShapeId(), y.getShapeId(), cx)) return REAL_CONFLICT;

			Object aux = x;
			x = y;
			y = (Operation) aux;


			aux = cx;
			cx = cy;
			cy = (GRootGroup) aux;

		}

		if (x instanceof GroupOperation && y instanceof GroupOperation)
		{
			GroupOperation grx = (GroupOperation) x;
			GroupOperation gry = (GroupOperation) y;
			for (int i = 0; i < grx.getChildren().size(); i++)
				for (int j = 0; j < gry.getChildren().size(); j++)
				{
					if (((GShape) grx.getChildren().get(i)).getId().equals(((GShape) gry.getChildren().get(j)).getId()))
						return REAL_CONFLICT;
					GShape shapex = (GShape) grx.getChildren().get(i);
					GShape shapey = (GShape) gry.getChildren().get(j);

					if (shapex.existsId(shapey.getId())) return REAL_CONFLICT;
					if (shapey.existsId(shapex.getId())) return REAL_CONFLICT;					
				}
		}


		x = op1;
		y = op2;
		cx = remoteContext;
		cy = localContext;

		int result = SOLVABLE_REVERSE_ORDER_CONFLICT;
		for (int i = 0; i < 2; i++)
		{
			if (x instanceof GroupOperation && y instanceof DeleteOperation)
			{
				GroupOperation gop = (GroupOperation) x;
				for (int j = 0; j < gop.getChildren().size(); j++)
					if (((GShape) gop.getChildren().get(j)).getId().equals(y.getShapeId()))
						return result;
			}

			if (x instanceof GroupOperation && y instanceof UngroupOperation)
			{
				GroupOperation gop = (GroupOperation) x;
				for (int j = 0; j < gop.getChildren().size(); j++)
					if (((GShape) gop.getChildren().get(j)).getId().equals(y.getShapeId()))
						return result;
			}


			if (x instanceof SetFgColorOperation && y instanceof SetFgColorOperation
				&& isParent(x.getShapeId(), y.getShapeId(), cx))
				return result;

			if (x instanceof SetFgColorOperation && y instanceof UngroupOperation &&
				x.getShapeId().equals(y.getShapeId()))
				return result;

			if (x instanceof SetBgColorOperation && y instanceof SetBgColorOperation
				&& isParent(x.getShapeId(), y.getShapeId(), cx))
				return result;

			if (x instanceof SetBgColorOperation && y instanceof UngroupOperation &&
				x.getShapeId().equals(y.getShapeId()))
				return result;

			if (x instanceof TranslationOperation && y instanceof UngroupOperation &&
				x.getShapeId().equals(y.getShapeId()))
				return result;

			if (x instanceof ScaleOperation && y instanceof UngroupOperation &&
				x.getShapeId().equals(y.getShapeId()))
				return result;


			if (x instanceof SetZOperation && y instanceof UngroupOperation &&
				x.getShapeId().equals(y.getShapeId()))
				return result;




			result = SOLVABLE_RIGHT_ORDER_CONFLICT;

			Object aux;

			aux = x;
			x = y;
			y = (Operation) aux;

			aux = cx;
			cx = cy;
			cy = (GRootGroup) aux;
		}

		return NO_CONFLICT;

	}

	public void compress(ArrayList list, GRootGroup root)
	{
		boolean bb;

		System.out.println("<beforecompress>");
		for (int i = 0; i < list.size(); i++)
			System.out.println(list.get(i));
		System.out.println("</beforecompress>");



		do
		{

			bb = false;


			GRootGroup gr = (GRootGroup) root.clone();
			for (int i = 0; i < list.size(); i++)
			{
				//setFgColorOperation
				if (list.get(i) instanceof SetFgColorOperation)
				{
                        GRootGroup grx = (GRootGroup) gr.clone();
						SetFgColorOperation opi = (SetFgColorOperation) list.get(i);

						ArrayList leafs = grx.getShape(opi.getShapeId()).getLeafs();
						for (int w = 0; w < leafs.size(); w++)
							if (grx.getShape((String) leafs.get(w)).getFgColor().equals(opi.getColor()))
							{
								leafs.remove(w); w--;
							}
						if (leafs.size() == 0)	{	list.remove(i); i--; bb = true;	break;}

						for (int j = i + 1; j < list.size(); j++)
						{
							if (list.get(j) instanceof SetFgColorOperation)
							{
								SetFgColorOperation opj = (SetFgColorOperation) list.get(j);
								for (int w = 0; w < leafs.size(); w++)
									if (grx.getShape(opj.getShapeId()).existsId((String) leafs.get(w)))
									{
										leafs.remove(w); w--;
									}
								if (leafs.size() == 0)
									{
										if (opi.isSynched()) opj.addIntegratedOps(opi.getIntegratedOps());
										list.remove(i); i--; bb = true;	break;
									}
							}
                	        ((Operation) list.get(j)).applyTo(grx);
						}
					if (bb) break;

				}


			   //setBgColorOperation
				else if (list.get(i) instanceof SetBgColorOperation)
				{
                        GRootGroup grx = (GRootGroup) gr.clone();
						SetBgColorOperation opi = (SetBgColorOperation) list.get(i);

						ArrayList leafs = grx.getShape(opi.getShapeId()).getLeafs();
						for (int w = 0; w < leafs.size(); w++)
							if (grx.getShape((String) leafs.get(w)).getBgColor().equals(opi.getColor()))
							{
								leafs.remove(w);		w--;
							}
						if (leafs.size() == 0)	{	list.remove(i); i--; bb = true;	break;}

						for (int j = i + 1; j < list.size(); j++)
						{
							if (list.get(j) instanceof SetBgColorOperation)
							{
								SetBgColorOperation opj = (SetBgColorOperation) list.get(j);
								for (int w = 0; w < leafs.size(); w++)
									if (grx.getShape(opj.getShapeId()).existsId((String) leafs.get(w)))
									{
										leafs.remove(w); w--;
									}
								if (leafs.size() == 0)
									{
										if (opi.isSynched()) opj.addIntegratedOps(opi.getIntegratedOps());
										list.remove(i); i--; bb = true;	break;
									}
							}
                	        ((Operation) list.get(j)).applyTo(grx);
						}

					if (bb) break;

				}

 			    //translate
				else if (list.get(i) instanceof TranslationOperation)
				{
					TranslationOperation ti = (TranslationOperation) list.get(i);

					if (Math.abs(ti.getDx()) < 0.00001 && Math.abs(ti.getDy()) < 0.00001)
					{
						list.remove(i); i--; bb = true; break;
					}

					for (int j = i + 1; j < list.size(); j++)
						if (list.get(i) instanceof TranslationOperation && list.get(j) instanceof TranslationOperation)
						{
							TranslationOperation tj = (TranslationOperation) list.get(j);

							if (!ti.getShapeId().equals(tj.getShapeId())) continue;

							String shapeId = ti.getShapeId();

							tj.setDx(tj.getDx() + ti.getDx());
							tj.setDy(tj.getDy() + ti.getDy());
							if (ti.isSynched())
								tj.addIntegratedOps(ti.getIntegratedOps());

							GGroup grp = null;
							if (gr.getShape(shapeId) instanceof GGroup)
								grp = (GGroup) gr.getShape(shapeId);

							for (int k = i + 1; k < j; k++)
							{
							    if (list.get(k) instanceof CreateOperation && grp != null)
							    {
								    CreateOperation cop = (CreateOperation) list.get(k);
								    if (grp.existsId(cop.getParentId()))
									    cop.getShape().moveWith(-ti.getDx(), -ti.getDy());

							    }
								((Operation)list.get(k)).applyTo(gr);
							}
							list.remove(i);	i--; bb = true;	break;
						}
					if (bb) break;
				}

				//scale
				else if (list.get(i) instanceof ScaleOperation)
				{
					ScaleOperation si = (ScaleOperation) ((ScaleOperation) list.get(i)).clone();

					if (Math.abs(si.getTx() - 1) < 0.00001 && Math.abs(si.getTy() - 1) < 0.00001)
					{
						list.remove(i); i--; bb = true; break;
					}

					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(si.getShapeId()))
						{
							if (list.get(j) instanceof ScaleOperation)
							{
								ScaleOperation sj = (ScaleOperation) list.get(j);
								System.out.println("si = " + si);
								System.out.println("sj = " + sj);
								if (Math.abs(sj.getRefx() - si.getRefx() + si.getObjrfx() - sj.getObjrfx()) <= 0.001 &&
								        Math.abs(sj.getRefy() - si.getRefy() + si.getObjrfy() - sj.getObjrfy()) <= 0.001)
								{

									String shapeId = si.getShapeId();

									GGroup grp = null;
									if (gr.getShape(shapeId) instanceof GGroup)
										grp = (GGroup) gr.getShape(shapeId);
									if (grp != null)
										for (int k = i + 1; k < j; k++)
										    if (list.get(k) instanceof CreateOperation)
										    {
											    CreateOperation cop = (CreateOperation) list.get(k);
											    if (grp.existsId(cop.getParentId()))
												    cop.getShape().scale(si.getRefx() - (si.getObjrfx() - grp.getRfx()),
												                         si.getRefy() - (si.getObjrfy() - grp.getRfy()),
												                           1 / si.getTx(), 1 / si.getTy());
										    }
									sj.setTx(sj.getTx() * si.getTx());
									sj.setTy(sj.getTy() * si.getTy());

									if (si.isSynched())
										sj.addIntegratedOps(si.getIntegratedOps());
									list.remove(i); i--; bb = true;	break;
								}
							}

						}
					if (bb) break;
				}

				// group
				else if (list.get(i) instanceof GroupOperation)
				{
					GroupOperation gi = (GroupOperation) list.get(i);
					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(gi.getShapeId()))
						{
							if (list.get(j) instanceof UngroupOperation)
							{
								list.remove(j); list.remove(i); i--;  bb = true; break;
							}
							break;
						}
					if (bb) break;
				}

				// ungroup
				else if (list.get(i) instanceof UngroupOperation)
				{
					UngroupOperation gi = (UngroupOperation) list.get(i);
					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(gi.getShapeId()))
						{
							if (list.get(j) instanceof GroupOperation)
							{
								list.remove(j); list.remove(i); i--;  bb = true;	break;
							}
							break;
						}
					if (bb) break;
				}

				//delete
				else if (list.get(i) instanceof DeleteOperation)
				{
					DeleteOperation gi = (DeleteOperation) list.get(i);
					for (int j = i + 1; j < list.size(); j++)
					{
						if (((Operation) (list.get(j))).getShapeId().equals(gi.getShapeId()))
						{
							if (list.get(j) instanceof CreateOperation)
							{
								list.remove(j); list.remove(i); i--;  bb = true;	break;
							}
							break;
						}
					}
					if (bb) break;
				}

				//create
				else if (list.get(i) instanceof CreateOperation)
				{
					CreateOperation gi = (CreateOperation) list.get(i);
					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(gi.getShapeId()))
						{
							if (list.get(j) instanceof DeleteOperation)
							{
								list.remove(j); list.remove(i); i--;  bb = true; break;
							}
							break;
						}
					if (bb) break;
				}

				//set text
				else if (list.get(i) instanceof SetTextOperation)
				{
					SetTextOperation st = (SetTextOperation) list.get(i);
					GText tshape = (GText) gr.getShape(st.getShapeId());
					if (tshape.getText().equals(st.getText()))
					{
								list.remove(i); i--;  bb = true; break;						
					}
					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(st.getShapeId()))
						{
							if (list.get(j) instanceof SetTextOperation)
							{
									if (st.isSynched())
											((Operation) (list.get(j))).addIntegratedOps(st.getIntegratedOps());
									list.remove(i); i--;  bb = true; break;
							}
						}
					if (bb) break;
				}

				//setZ
				else if (list.get(i) instanceof SetZOperation)
				{
					SetZOperation sz = (SetZOperation) list.get(i);
					GShape shape = gr.getShape(sz.getShapeId());
					if (shape.getZ() == sz.getZ())
					{
								list.remove(i); i--;  bb = true; break;
					}
					for (int j = i + 1; j < list.size(); j++)
						if (((Operation) (list.get(j))).getShapeId().equals(sz.getShapeId()))
						{
							if (list.get(j) instanceof SetZOperation)
							{
									if (sz.isSynched())
											((Operation) (list.get(j))).addIntegratedOps(sz.getIntegratedOps());
									list.remove(i); i--;  bb = true; break;
							}
						}
					if (bb) break;
				}


				//Nop
				else if (list.get(i) instanceof NopOperation)
				{
					list.remove(i); i--; bb = true; break;
				}

				((Operation)list.get(i)).applyTo(gr);


			}

		}while (bb);


		System.out.println("<compress>");
		for (int i = 0; i < list.size(); i++)
			System.out.println(list.get(i));
		System.out.println("</compress>");


	}

	public void checkOutVersion(int checkOutVersion)
	{
		String url = "rmi://" + rmiServer + ":" + rmiPort + "/repository";

		try
		{
			Repository remoteRef = (Repository) Naming.lookup(url);

			root = new GRootGroup();
			selection = new ArrayList();
			currentVersion = 0;
			firstupdate = true;
			lastUpdateRoot = new GRootGroup();
			history = new ArrayList();
			wasMultiselected = false;

			for (int i = getCurrentVersion(); i < checkOutVersion; i++)
			{
				ArrayList remoteLog = remoteRef.getOperations(i, i + 1);
				internalUpdate(remoteLog);
				setCurrentVersion(getCurrentVersion() + 1);
			}

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