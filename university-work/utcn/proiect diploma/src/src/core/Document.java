package core;

import utils.Debugger;

import java.util.ArrayList;
import java.util.Observable;
import java.awt.Frame;

import enum.OperationType;
import enum.UpdateType;
import enum.TreeLevel;
import data.DocumentTreeNode;
import gui.AbsoluteOperation;
import gui.diag.ChoiceDialog;
import data.DocumentTreeNode;

public class Document extends Observable
{
	//public Document(String text, Frame parentFrame, JTextPane textPane)
	public Document(String text, Frame parentFrame)
	{
		//this.textPane = textPane;

		this.currentVersion = 0;
		this.parentFrame = parentFrame;
		this.disableObservers = false;

		if (text.equals(""))
		{
			root = new DocumentTreeNode();
			root.setLevel(TreeLevel.document);
		}
		else
		{
			root = Parser.parseDocument(text);
		}
	}

	/**
	 * Returns the contents of the document as a String.
	 */
	public String getText()
	{
		return root.toString();
	}

	public int getCurrentVersion()
	{
		return currentVersion;
	}

	public void setCurrentVersion(int currentVersion)
	{
		this.currentVersion = currentVersion;
	}

	/**
	 * Inserts character <code>c</code> in position <code>position</code> in the document.
	 *
	 * @param c the character to insert
	 * @param position the absolute position to insert it in
	 */
	public void insertChar(char c, int position)
	{
		ArrayList index = new ArrayList();
		insertChar(c, position, root, index);
	}

	/**
	 * Deletes the character in position <code>position</code> from the document.
	 *
	 * @param position the absolute position of the character to delete
	 */
	public void deleteChar(int position)
	{
		ArrayList index = new ArrayList();
		deleteChar(position, root, index);
	}

	/**
	 * Executes <code>operation</code> on this document and optionally adds it to the log.
	 *
	 * @param operation the operation to apply
	 * @param addToLog addToLog true if you also want to log the operation, false otherwise.
	 */
	public void applyOperation(Operation operation, boolean addToLog)
	{
		applyOperation(operation, root, addToLog);
	}

	/**
	 * This method updates the local document with the operations from the repository. The <code>remoteLog</code>
	 * has to be ordered by the operation level (with the higher level operations first). Otherwise, the method
	 * doesn't work and you will probably end up with a corrupt document.
	 *
	 * @param remoteLog the reomte log from the repository (should represent the difference between two
	 *      consecutive versions)
	 */
	public void update(ArrayList remoteLog, int level, int updateType)
	{
		reduceOperations();
		update(root, remoteLog, level, updateType);
		addSeparators();
	}

	/**
	 * Integrates all operations from <code>remoteLog</code> into this document tree. The only difference between
	 * this method and the {@link #update(java.util.ArrayList, int, int)} update} method is that this one doesn't
	 * modify the local log in order to include the effect of the operations in <code>remoteLog</code> (as is the
	 * case with update). It simply executes and adds the operations from the <code>remoteLog</code> to the local
	 * log.
	 *
	 * @param remoteLog the list of remote operations to integrate
	 * @param asMaster if true, this host will be master during the synchronization, if false, this host will be
	 *      slave during synchronization. This parameter is only taken into consideration if
	 *      <code>updateType</code> is UpdateType.noChoice
	 */
	public void synchronize(ArrayList remoteLog, int level, int updateType, boolean asMaster)
	{
		reduceOperations();
		synchronize(root, remoteLog, level, updateType, asMaster);
		addSeparators();
	}

	/**
	 * Returns the commit log (a list of operations representing the delta between the current version of the
	 * document and the last updated version). The log results from a breadth first traversal of the document
	 * tree.
	 */
	public ArrayList getCommitLog()
	{
		reduceOperations();

		correctOpsIndex();

		ArrayList result = new ArrayList();

		ArrayList nodeList = new ArrayList();

		nodeList.add(root);
		int k = 0;

		while (k < nodeList.size())
		{
			DocumentTreeNode node = (DocumentTreeNode) nodeList.get(k);
			ArrayList log = node.getLog();
			for (int i = 0; i < log.size(); i++)
			{
				result.add(((Operation) log.get(i)).clone());
			}

			for (int i = 0; i < node.getNrChildren(); i++)
			{
				nodeList.add(node.getChildAt(i));
			}

			k++;
		}

		return result;
	}

	/**
	 * Returns all operations from the local log after locking them all to prevent them from taking part in any
	 * future reduce operations.
	 *
	 * @return the list of operations from the enitre tree
	 */
	public ArrayList getSynchOperations()
	{
		reduceOperations();
		lockAll();
		return getCommitLog();
	}

	/**
	 * Reduces the list of operations by combining several lower level operations in a single higher level
	 * operation. Make sure to call the {@link #correctOpsIndex() correctOpsIndex} method after reducing the
	 * operations in order to correct their indices (as they might become invalid after the reduction).
	 */
	public void reduceOperations()
	{
		reduceOperations(root);
	}

	/**
	 * Empties the log of all tree nodes.
	 */
	public void emptyLog()
	{
		emptyLog(root);
	}

	/**
	 * The private version of deleteChar. Does the actual work.
	 *
	 * @param node should be called with the root node
	 * @param index should be called with the an empty ArrayList
	 */
	private void deleteChar(int position, DocumentTreeNode node, ArrayList index)
	{
		int k = 0;

		if (node.getLevel() == TreeLevel.word)
		{
			index.add(new Integer(position));
			applyOperation(new Operation(OperationType.delete, index, "" +
				node.getContent().charAt(position)), true);
			return;
		}

		for (int childNo = 0; childNo < node.getNrChildren(); childNo++)
		{
			DocumentTreeNode child = node.getChildAt(childNo);
			index.add(new Integer(childNo));
			if (k <= position && position < k + child.getLength())
			{
				if (child.isSeparator())
				{
					if (childNo == 0 || childNo == node.getNrChildren() - 1)
					{
						applyOperation(new Operation(OperationType.delete, index,
							child.toString()), true);
						return;
					}

					DocumentTreeNode prev = node.getChildAt(childNo - 1);
					DocumentTreeNode next = node.getChildAt(childNo + 1);
					if (prev.isSeparator() || next.isSeparator())
					{
						applyOperation(new Operation(OperationType.delete, index,
							child.toString()), true);
						return;
					}

					// A merge is done as three delete's and an insert
					String text = prev.toString() + next.toString();
					index.remove(index.size() - 1);
					index.add(new Integer(childNo - 1));

					applyOperation(new Operation(OperationType.delete, index, prev.toString()), true);
					applyOperation(new Operation(OperationType.delete, index, child.toString()), true);
					applyOperation(new Operation(OperationType.delete, index, next.toString()), true);

					applyOperation(new Operation(OperationType.insert, index, text), true);
					return;
				}

				if (child.getLength() > 1)
				{
					deleteChar(position - k, child, index);
				}
				else
				{
					applyOperation(new Operation(OperationType.delete, index, child.toString()), true);
				}
				return;
			}
			index.remove(index.size() - 1);
			k += child.getLength();
		}
	}

	/**
	 * The private version of insertChar. Does the actual work.
	 *
	 * @param node should be called with the root node
	 * @param index should be called with the an empty ArrayList
	 */
	private void insertChar(char c, int position, DocumentTreeNode node, ArrayList index)
	{
		if ((node.getLevel() == TreeLevel.document && Parser.paragraphSeparators.indexOf(c) != -1) || //  paragraph separator
			(node.getLevel() == TreeLevel.paragraph && Parser.sentenceSeparators.indexOf(c) != -1) || //sentence separator
			(node.getLevel() == TreeLevel.sentence && Parser.wordSeparatos.indexOf(c) != -1)) // word separator
		{
			DocumentTreeNode child;
			int k = 0;
			for (int childNo = 0; childNo < node.getNrChildren(); childNo++)
			{
				index.add(new Integer(childNo));
				child = node.getChildAt(childNo);
				if (k == position) // insertion
				{
					applyOperation(new Operation(OperationType.insert, index, "" + c), true);
					return;
				}

				int l = child.getLength();
				if (k < position && position < k + l) // a split
				{
					// A split is executed as a remove and three new inserts
					String text = child.toString();

					applyOperation(new Operation(OperationType.delete, index, text), true);

					int w; // spliting point
					w = position - k;

					String text1;
					String text2;

					text1 = text.substring(0, w);
					text2 = text.substring(w);

					applyOperation(new Operation(OperationType.insert, index, text2), true);
					applyOperation(new Operation(OperationType.insert, index, "" + c), true);
					applyOperation(new Operation(OperationType.insert, index, text1), true);
					return;
				}

				index.remove(index.size() - 1);
				k += child.getLength();
			}

			index.add(new Integer(node.getNrChildren()));
			applyOperation(new Operation(OperationType.insert, index, "" + c), true);
			return;
		}

		if (node.getLevel() == TreeLevel.word)
		{
			index.add(new Integer(position));
			applyOperation(new Operation(OperationType.insert, index, "" + c), true);
			return;
		}

		int k = 0;
		for (int childNo = 0; childNo < node.getNrChildren(); childNo++)
		{
			index.add(new Integer(childNo));
			DocumentTreeNode child = node.getChildAt(childNo);
			if (!child.isSeparator())
			{
				if (k <= position && position <= k + child.getLength())
				{
					insertChar(c, position - k, child, index);
					return;
				}
			}
			else if (position == k)
			{
				applyOperation(new Operation(OperationType.insert, index, "" + c), true);
				return;
			}

			k += child.getLength();
			index.remove(index.size() - 1);
		}

		if (node.getNrChildren() == 0)
		{
			index.add(new Integer(0));
			applyOperation(new Operation(OperationType.insert, index, "" + c), true);
			return;
		}
		else
		{
			DocumentTreeNode lastchild;
			lastchild = node.getChildAt(node.getNrChildren() - 1);
			if (lastchild.isSeparator() && position == node.getLength())
			{
				index.add(new Integer(node.getNrChildren()));
				applyOperation(new Operation(OperationType.insert, index, "" + c), true);
				return;
			}
		}
	}

	/**
	 * The private version of applyOperation. Does the actual work.
	 *
	 * @param op the operation to apply
	 * @param node should be called with the root node
	 * @param addToLog true if you also want to log the operation, false otherwise
	 */
	private void applyOperation(Operation op, DocumentTreeNode node, boolean addToLog)
	{
		if (op.getType() == OperationType.nop)
		{
			return;
		}

		// find out which child of the current node the operations refers to
		int idx[] = op.getIndex();
		int childNo;
		switch (node.getLevel())
		{
			case TreeLevel.document:
				childNo = idx[0];
				break;
			case TreeLevel.paragraph:
				childNo = idx[1];
				break;
			case TreeLevel.sentence:
				childNo = idx[2];
				break;
			case TreeLevel.word:
				childNo = idx[3];
				break;
			default:
				Debugger.getInstance().postMessage("Document.applyOperation: tree level of node was " +
				        node.getLevel());
				return;
		}

		if (op.getLevel() != node.getLevel())
		{
//			DocumentTreeNode child;
//			try
//			{
//			}
//			catch (Exception e)
//			{
//				return;
//			}
			applyOperation(op, node.getChildAt(childNo), addToLog);
		}
		else
		{
			// this is used in order to notify observers
			AbsoluteOperation absOp = new AbsoluteOperation();
			absOp.setType(op.getType());
			absOp.setSourceIp(op.getIp());

			if (op.getType() == OperationType.insert)
			{
				absOp.setContent(op.getContent());
				if (node.getLevel() == TreeLevel.word)
				{
					String oldContent = node.getContent();
					node.setContent(oldContent.substring(0, childNo) +
					        op.getContent() + oldContent.substring(childNo));

					absOp.setPosition(getPosition(node) + childNo);
				}
				else
				{
					if (node.getLevel() == TreeLevel.sentence)
					{
						node.addChildAt(childNo, Parser.parseWord(op.getContent()));
					}
					else if (node.getLevel() == TreeLevel.paragraph)
					{
						node.addChildAt(childNo, Parser.parseSentence(op.getContent()));
					}
					else if (node.getLevel() == TreeLevel.document)
					{
						node.addChildAt(childNo, Parser.parseParagraph(op.getContent()));
					}

					absOp.setPosition(getPosition(node.getChildAt(childNo)));
				}
			}
			else if (op.getType() == OperationType.delete)
			{
				if (node.getLevel() == TreeLevel.word)
				{
					absOp.setContent(op.getContent());
					absOp.setPosition(getPosition(node) + childNo);

					String oldContent = node.getContent();
					node.setContent(oldContent.substring(0, childNo) +
						oldContent.substring(childNo + 1));
				}
				else
				{
					undoOperations(node.getChildAt(childNo));
					op.setContent(node.getChildAt(childNo).toString());

					absOp.setContent(node.getChildAt(childNo).toString());
					absOp.setPosition(getPosition(node.getChildAt(childNo)));

					node.removeChild(childNo);
				}
			}

			if (addToLog)
			{
				Debugger.getInstance().postMessage("Document.applyOperation: " + op.toString());
				node.addOperationToLog(op);
			}

			if (!disableObservers)
			{
				setChanged();
				notifyObservers(absOp);
			}
		}
	}

	/**
	 * This is the private version of the update method. Does the actual work.
	 *
	 * @param node should be called with the root node
	 * @param remoteLog the remote log from the reopository
	 * @param level the conflict level to be used (word level, sentence level, paragraph level)
	 * @param updateType the conflict resolution method to use
	 */
	private void update(DocumentTreeNode node, ArrayList remoteLog, int level, int updateType)
	{
		synch_update(node, remoteLog, level, true, updateType, true);
	}

	/**
	 * This is the private version of the synchronize method. Does the actual work.
	 *
	 * @param node should be called with the root node
	 * @param remoteLog the remote log to integrate
	 * @param level the conflict level to be used (word level, sentence level, paragraph level)
	 * @param updateType the conflict resolution method to use
	 * @param asMaster if true, this host will be master during the synchronization, if false, this host will be
	 *      slave during synchronization. This parameter is only taken into consideration if
	 *      <code>updateType</code> is UpdateType.noChoice
	 */
	private void synchronize(DocumentTreeNode node, ArrayList remoteLog, int level, int updateType,
	        boolean asMaster)
	{
		synch_update(node, remoteLog, level, asMaster, updateType, false);
	}

	/**
	 * @param isUpdate if true, functions as update, if false, functions as synchronize
	 */
	private void synch_update(DocumentTreeNode root, ArrayList remoteLog, int level, boolean keeplocalversion,
		int updateType, boolean isUpdate)
	{
		// cloning remoteLog; just to be sure
		ArrayList remoteLogclone = new ArrayList();
		for (int i = 0; i < remoteLog.size(); i++)
			remoteLogclone.add(((Operation) remoteLog.get(i)).clone());
		remoteLog = remoteLogclone;

		ArrayList remoteLevelLog = new ArrayList();
		int borderIndex = remoteLog.size();
		for (int i = 0; i < remoteLog.size(); i++)
		{
			Operation op = (Operation) remoteLog.get(i);
			if (op.getLevel() == root.getLevel())
			{
				remoteLevelLog.add(op.clone());
			}
			else
			{
				borderIndex = i;
				i = remoteLog.size();
			}
		}

		ArrayList localLog = (ArrayList) root.getLog().clone();
		for (int i = 0; i < localLog.size(); i++)
			localLog.set(i, ((Operation) localLog.get(i)).clone());

		int indices[] = root.getIndices();
		if (indices != null)	// this means we are at the very root of the document tree
		{
			int nrIndices = indices.length;
			for (int i = 0; i < localLog.size(); i++)
			{
				Operation op = (Operation) localLog.get(i);
				op.setPIdx(indices[0]);
				if (nrIndices > 1)
				{
					op.setSIdx(indices[1]);
				}
				if (nrIndices > 2)
				{
					op.setWIdx(indices[2]);
				}
			}
		}

		ArrayList newRemoteLog = new ArrayList();
		ArrayList newLocalLog = new ArrayList();

		switch (updateType)
		{
			case UpdateType.noMerge:
				if (keeplocalversion)
				{
					if (localLog.isEmpty())
						newLocalLog = inverse(remoteLevelLog);
					else
						mergeNoChoice(level, remoteLevelLog, localLog, newRemoteLog, newLocalLog);
				}
				else
				{
					if (remoteLevelLog.isEmpty())
						newRemoteLog = inverse(localLog);
					else
						mergeNoChoice(level, localLog, remoteLevelLog, newLocalLog, newRemoteLog);
				}
				break;
			case UpdateType.noChoice:
			case UpdateType.conflictChoice:
				if (keeplocalversion)
					mergeNoChoice(level, remoteLevelLog, localLog, newRemoteLog, newLocalLog);
				else
					mergeNoChoice(level, localLog, remoteLevelLog, newLocalLog, newRemoteLog);
				break;
			case UpdateType.operationChoice:
				mergeOperationChoice(level, localLog, remoteLevelLog, newLocalLog, newRemoteLog);
				break;
		}

		if (isUpdate)
		{
			root.emptyLog();
		}
		for (int i = 0; i < newRemoteLog.size(); i++)
		{
			applyOperation((Operation) newRemoteLog.get(i), !isUpdate);
		}

		if (isUpdate)
		{
			for (int i = 0; i < newLocalLog.size(); i++)
			{
				root.addOperationToLog((Operation) newLocalLog.get(i));
			}
		}

		ArrayList[] childRemoteLog = new ArrayList[root.getNrChildren()];

		for (int i = 0; i < root.getNrChildren(); i++)
		{
			childRemoteLog[i] = new ArrayList();
		}

		int indexNr = -1;

		switch (root.getLevel())
		{
			case TreeLevel.document:
				indexNr = 0;
				break;
			case TreeLevel.paragraph:
				indexNr = 1;
				break;
			case TreeLevel.sentence:
				indexNr = 2;
				break;
			case TreeLevel.word:
				indexNr = 3;
				break;
		}

		for (int i = borderIndex; i < remoteLog.size(); i++)
		{
			Operation op = (Operation) remoteLog.get(i);
			for (int j = 0; j < newLocalLog.size(); j++)
			{
				op = op.include((Operation) newLocalLog.get(j));
			}
			remoteLog.set(i, op);	// is this really necessary?

			if (op.getType() != OperationType.nop)
			{
				int[] opIndex = op.getIndex();
				childRemoteLog[opIndex[indexNr]].add(op);
			}
		}

		for (int i = 0; i < root.getNrChildren(); i++)
		{
			if (root.getChildAt(i).getLevel() != level)
				synch_update(root.getChildAt(i), childRemoteLog[i], level, keeplocalversion, updateType, isUpdate);
			else
			{
				if (childRemoteLog[i].size() == 0)
				{
					synch_update(root.getChildAt(i), childRemoteLog[i], level, true, UpdateType.noMerge, isUpdate);
					continue;
				}
				else if (isLogEmpty(root.getChildAt(i)))
				{
					synch_update(root.getChildAt(i), childRemoteLog[i], level, false, UpdateType.noMerge, isUpdate);
					continue;
				}

				if (updateType == UpdateType.noChoice || updateType == UpdateType.noMerge)
				{
					synch_update(root.getChildAt(i), childRemoteLog[i], level, keeplocalversion, UpdateType.noMerge, isUpdate);
					continue;
				}
				else if (updateType == UpdateType.operationChoice)
				{
					synch_update(root.getChildAt(i), childRemoteLog[i], level, true, UpdateType.operationChoice, isUpdate);
					continue;
				}

				DocumentTreeNode childi = (DocumentTreeNode) root.getChildAt(i).clone();

				disableObservers = true;

				synch_update(root.getChildAt(i), childRemoteLog[i], level, true, UpdateType.noMerge, isUpdate);

				int select_begin = getPosition(root.getChildAt(i));
				int select_end  = select_begin + root.getChildAt(i).getLength();

				DocumentTreeNode tree1 = root.getChildAt(i);
				String version1 = tree1.toString();

				root.setChildAt(i, (DocumentTreeNode) childi.clone());
				synch_update(root.getChildAt(i), childRemoteLog[i], level, false, UpdateType.noMerge, isUpdate);
				DocumentTreeNode tree2 = root.getChildAt(i);
				String version2 = tree2.toString();

				disableObservers = false;

				if (version1.equals(version2))
				{
					//System.out.println("equals...");
					//System.out.println("version1:" + version1);
					//System.out.println("version2:" + version2);
					root.setChildAt(i, childi);
					synch_update(root.getChildAt(i), childRemoteLog[i], level, true, updateType, isUpdate);
					continue;
				}

				ChoiceDialog cd = new ChoiceDialog(parentFrame);
				cd.setChoice(1, version1);
				cd.setChoice(2, version2);

				int c;
				DocumentTreeNode viewnode = root.getChildAt(i);

				//textPane.setCaretPosition(select_begin);
				//textPane.moveCaretPosition(select_end);

				//textPane.repaint(500);

				//System.out.println("select_begin=" + select_begin + "\n select_end=" + select_end);

				do
				{
					cd.show();
					c = cd.getChoice();
					if (c == 0 && viewnode.getLevel() != TreeLevel.document)
					{
						viewnode = viewnode.getParent();

						root.setChildAt(i, tree1);
						version1 = viewnode.toString();
						cd.setChoice(1, version1);

						root.setChildAt(i, tree2);
						version2 = viewnode.toString();
						cd.setChoice(2, version2);
					}

				}
				while (c == 0);

				root.setChildAt(i, childi);
				if (c == 1)
					synch_update(root.getChildAt(i), childRemoteLog[i], level, true, UpdateType.noMerge, isUpdate);
				else
					synch_update(root.getChildAt(i), childRemoteLog[i], level, false, UpdateType.noMerge, isUpdate);

			}
		}
	}

	/**
	 * Takes two lists of {@link Operation Operation}'s (<code>remoteLog</code> and <code>localLog</code>) as input
	 * parameters and two empty lists (<code>newRemoteLog</code> and <code>newLocalLog</code>) as output parameters.
	 * The output lists will be filled with {@link Operation Operation}'s the following way:
	 * <code>newRemoteLog</code> will contain the operations that have to be executed locally in order to bring the
	 * current version of the local document to the version indicated in <code>remoteLog</code> while
	 * <code>newLocalLog</code> will contain the operations that have to replace those in <code>localLog</code>
	 * (they are versions of the operations in <code>localLog</code> that include the effect of the operations in
	 * <code>remoteLog</code>).
	 *
	 * @param level the level at which conflicts are defined
	 * @param remoteLog the remote log from the repository (will be modified during the execution of this method)
	 * @param localLog the local log (will be modified during the execution of this method)
	 * @param newRemoteLog the transformed <code>remoteLog</code> which includes the effects of the operations in
	 * <code>localLog</code>. The parameter should be initialized with an empty ArrayList
	 * @param newLocalLog the transformed <code>localLog</code> which includes the effects of the operations in
	 * <code>remoteLog</code>. The parameter should be initialized with an empty ArrayList
	 */
	private void mergeNoChoice(int level, ArrayList remoteLog, ArrayList localLog, ArrayList newRemoteLog,
		ArrayList newLocalLog)
	{
		ArrayList RL, LL, ERL, TLL;
		RL = remoteLog;
		LL = localLog;

		ERL = newRemoteLog;
		TLL = newLocalLog;

		for (int i = 0; i < RL.size(); i++)
		{
			// save LL into CLL
			ArrayList CLL = new ArrayList(LL.size());
			for (int k = 0; k < LL.size(); k++)
			{
				CLL.add(((Operation) LL.get(k)).clone());
			}

			// save RL[i] into CRLi
			Operation CRLi = (Operation) ((Operation) RL.get(i)).clone();

			int j;
			for (j = 0; j < LL.size(); j++)
			{
				if (semanticConflict(level, (Operation) RL.get(i), (Operation) LL.get(j)))
				{
					Debugger.getInstance().postMessage("Document.mergeNoChoice: conflict");

					// restore RL[i] from CRLi
					RL.set(i, CRLi);

					// restore LL from CLL
					LL = new ArrayList(CLL.size());
					for (int k = 0; k < CLL.size(); k++)
					{
						LL.add(((Operation) CLL.get(k)).clone());
					}

					Operation o = removeOperation(i, RL);
					i--;
					TLL.add(o.invert());
					break;
				}
				else
				{
					Operation.symInclusion((Operation) RL.get(i), (Operation) LL.get(j));
				}
			}

			if (j == LL.size())
			{
				ERL.add(((Operation) RL.get(i)).clone());
			}
		}

		for (int i = 0; i < LL.size(); i++)
		{
			TLL.add(((Operation) LL.get(i)).clone());
		}

		Debugger.getInstance().postMessage("Document.mergeNoChoice: <ERL>");
		for (int i = 0; i < ERL.size(); i++)
		{
			Debugger.getInstance().postMessage("Document.mergeNoChoice: " + ERL.get(i).toString());
		}
		Debugger.getInstance().postMessage("Document.mergeNoChoice: </ERL>");

		Debugger.getInstance().postMessage("Document.mergeNoChoice: <TLL>");
		for (int i = 0; i < TLL.size(); i++)
		{
			Debugger.getInstance().postMessage("Document.mergeNoChoice: " + TLL.get(i));
		}
		Debugger.getInstance().postMessage("Document.mergeNoChoice: <TLL>");
	}

	/**
	 * Takes two lists of {@link Operation Operation}'s (<code>remoteLog</code> and <code>localLog</code>) as input
	 * parameters and two empty lists (<code>newRemoteLog</code> and <code>newLocalLog</code>) as output parameters.
	 * The output lists will be filled with {@link Operation Operation}'s the following way:
	 * <code>newRemoteLog</code> will contain the operations that have to be executed locally in order to bring the
	 * current version of the local document to the version indicated in <code>remoteLog</code> while
	 * <code>newLocalLog</code> will contain the operations that have to replace those in <code>localLog</code>
	 * (they are versions of the operations in <code>localLog</code> that include the effect of the operations in
	 * <code>remoteLog</code>).
	 * <br /><br />
	 * The difference bewteen mergeOperationChoice and {@link #mergeNoChoice mergeNoChoice} is that this version
	 * asks the user to choose between the local and the remote operation whenever a conflict between the two
	 * arises ({@link #mergeNoChoice mergeNoChoice} always chooses the local operation over the remote one in case
	 * of conflict).
	 *
	 * @param level the level at which conflicts are defined
	 * @param remoteLog the remote log from the repository (will be modified during the execution of this method)
	 * @param localLog the local log (will be modified during the execution of this method)
	 * @param newRemoteLog the transformed <code>remoteLog</code> which includes the effects of the operations in
	 * <code>localLog</code>. The parameter should be initialized with an empty ArrayList
	 * @param newLocalLog the transformed <code>localLog</code> which includes the effects of the operations in
	 * <code>remoteLog</code>. The parameter should be initialized with an empty ArrayList
	 */
	private void mergeOperationChoice(int level, ArrayList remoteLog, ArrayList localLog, ArrayList newRemoteLog,
		ArrayList newLocalLog)
	{
		ArrayList RL, LL, ERL, TLL;
		RL = remoteLog;
		LL = localLog;

		ERL = newRemoteLog;
		TLL = newLocalLog;

		for (int i = 0; i < RL.size(); i++)
		{
			ArrayList CLL = new ArrayList(LL.size());
			for (int k = 0; k < LL.size(); k++)
			{
				CLL.add(((Operation) LL.get(k)).clone());
			}

			Operation CRLi = (Operation) ((Operation) RL.get(i)).clone();

			int j;
			for (j = 0; j < LL.size(); j++)
			{
				if (semanticConflict(level, (Operation) RL.get(i), (Operation) LL.get(j)))
				{
					// choice indicates to remove the remote operation
					if (choice(((Operation) RL.get(i)).toString(),
						((Operation) LL.get(j)).toString()) == 2)
					{
						RL.set(i, CRLi);
						LL = new ArrayList(CLL.size());
						for (int k = 0; k < CLL.size(); k++)
						{
							LL.add(((Operation) CLL.get(k)).clone());
						}
						Operation o = removeOperation(i, RL);
						i--;
						TLL.add(o.invert());
						break;
					}
					// choice indicates to remove the local operation
					else
					{
						Operation o = removeOperation(j, LL);
						CLL = (ArrayList) LL.clone();
						j--;
						ERL.add(o.invert());
					}
				}
				else
				{
					Operation.symInclusion((Operation) RL.get(i), (Operation) LL.get(j));
				}
			}

			if (j == LL.size())
			{
				ERL.add(((Operation) RL.get(i)).clone());
			}
		}

		for (int i = 0; i < LL.size(); i++)
		{
			TLL.add(((Operation) LL.get(i)).clone());
		}

		Debugger.getInstance().postMessage("Document.mergeNoChoice: <ERL>");
		for (int i = 0; i < ERL.size(); i++)
		{
			Debugger.getInstance().postMessage("Document.mergeNoChoice: " + ERL.get(i).toString());
		}
		Debugger.getInstance().postMessage("Document.mergeNoChoice: </ERL>");

		Debugger.getInstance().postMessage("Document.mergeNoChoice: <TLL>");
		for (int i = 0; i < TLL.size(); i++)
		{
			Debugger.getInstance().postMessage("Document.mergeNoChoice: " + TLL.get(i));
		}
		Debugger.getInstance().postMessage("Document.mergeNoChoice: <TLL>");
	}

	/**
	 * Decides whether operations <code>op1</code> and <code>op2</code> are in conflict. The two operations
	 * are in conflict if they refer to the same semantic unit defined by <code>level</code> (i.e. same
	 * word, same sentence, same paragraph).
	 * <br /><br />
	 * The required precondition before calling this method is that op1 and op2 are operations of the
	 * same level. If this is not true, the result will probably be wrong.
	 *
	 * @param level the level of conflict
	 * @param op1 the first operation
	 * @param op2 the second operation
	 * @return true if the two operations are in conflict, false if they are not in conflict
	 */
	private static boolean semanticConflict(int level, Operation op1, Operation op2)
	{
		if (op1.getType() == OperationType.nop || op2.getType() == OperationType.nop)
		{
			return false;
		}

		if (level <= op1.getLevel())
		{
			for (int i = 0; i < level; i++)
			{
				if (op1.getIndex()[i] != op2.getIndex()[i])
				{
					return false;
				}
			}

			return true;
		}
		else
		{
			return false;
		}
	}

	private int choice(String choice1, String choice2)
	{
		ChoiceDialog cDiag = new ChoiceDialog(parentFrame);

		cDiag.setChoice(1, choice1);
 		cDiag.setChoice(2, choice2);

		cDiag.show();

		return cDiag.getChoice();
	}

	/**
	 * Removes an operation and the effect of that operation form a list of operations.
	 *
	 * @param opIndex the index of the operation to remove
	 * @param list the list of operations
	 */
	private static Operation removeOperation(int opIndex, ArrayList list)
	{
		for (int i = opIndex; i < list.size() - 1; i++)
		{
			Operation.transpose((Operation) list.get(i), (Operation) list.get(i+1));
		}

		return (Operation) list.remove(list.size() - 1);
	}

	/**
	 * The private version of reduceOperations. Does the actual work.
	 *
	 * @param node should be called with the root node
	 */
	private void reduceOperations(DocumentTreeNode node)
	{

		if (node.getLevel() == TreeLevel.word)
		{
			return;
		}

		for (int i = 0; i < node.getNrChildren(); i++)
		{
			reduceOperations(node.getChildAt(i));
		}

		ArrayList log = node.getLog();
		for (int i = 0; i < log.size(); i++)
		{
			Operation op = (Operation) log.get(i);

			if (op.getType() == OperationType.delete || op.isLocked())
			{
				continue;
			}

			if (op.getType() == OperationType.nop)
			{
				log.remove(i);
				i--;
				continue;
			}

			Operation opClone = (Operation) op.clone();

			int level = node.getLevel();

			// Finds the current positon of the inserted element. This position could
			// differ from the original one as a result of further insertions or deletions.
			for (int j = i+1; j < log.size(); j++)
			{
				Operation opj = (Operation) log.get(j);
				int position = opj.getIndex()[level];

				if (opj.getType() == OperationType.insert)
				{
					if (position <= opClone.getIndex()[level])
					{
						opClone.getIndex()[level]++;
					}
				}
				if (opj.getType() == OperationType.delete)
				{
					if (position < opClone.getIndex()[level])
					{
						opClone.getIndex()[level]--;
					}
					else if (position == opClone.getIndex()[level])
					{
						opClone.setType(OperationType.nop);
						break;
					}
				}
			}

			if (opClone.getType() == OperationType.nop)
			{
				continue;
			}

			int childNo = opClone.getIndex()[node.getLevel()];
			op.setContent(node.getChildAt(childNo).toString());
			emptyLog(node.getChildAt(childNo));
		}
	}

	/**
	 * Corrects the indices of all operations in the logs of all the nodes of the docuement tree. This is
	 * usually necessary after the {@link #reduceOperations reduceOperations} method is called, as some of
	 * the indices will no longer be valid after such an operation.
	 */
	private void correctOpsIndex()
	{
		ArrayList index = new ArrayList();
		correctOpsIndex(root, index);
	}

	/**
	 * Corrects the indexes of all operations in the logs of all the nodes in the subtree which has
	 * <code>node</code> as its root.
	 *
	 * @param node should be called with the root node
	 * @param index should be called with an empty ArrayList
	 */
	private void correctOpsIndex(DocumentTreeNode node, ArrayList index)
	{
		ArrayList log = node.getLog();
		for (int i = 0; i < log.size(); i++)
		{
			Operation op = (Operation) log.get(i);
			for (int j = 0; j < index.size(); j++)
				op.getIndex()[j] = ((Integer) index.get(j)).intValue();
		}

		for (int i = 0; i < node.getNrChildren(); i++)
		{
			index.add(new Integer(i));
			correctOpsIndex(node.getChildAt(i), index);
			index.remove(index.size() - 1);
		}

	}

	/**
	 * Returns the absolute postion of the begining of <code>node</code> in the text.
	 *
	 * @param node the node whose position you want to find
	 */
	private int getPosition(DocumentTreeNode node)
	{
		if (node == root || node.getParent() == null)
		{
			return 0;
		}

		int childNo = node.getChildNo();
		int position = getPosition(node.getParent());

		if (childNo >= node.getParent().getNrChildren())
		{
			Debugger.getInstance().postMessage("Document.getPosition: parent position = " +
				getPosition(node.getParent()));
			Debugger.getInstance().postMessage("Document.getPosition: child number = " +
				childNo + " of " + node.getParent().getNrChildren());

			for (int i = 0; i < node.getParent().getNrChildren(); i++)
			{
				Debugger.getInstance().postMessage("Document.getPosition: " +
					node.getParent().getChildAt(i).getChildNo());
			}
		}

		for (int i = 0; i < childNo; i++)
		{
			position += node.getParent().getChildAt(i).getLength();
		}

		return position;
	}

	/**
	 * Applies the inverse of all operations at all tree levels, starting with the <code>node</code>
	 * received as parameter, from back to front.
	 *
	 * @param node the root of the subtree which has to have its operations undone
	 */
	private void undoOperations(DocumentTreeNode node)
	{
		ArrayList log = node.getLog();
		for (int i = log.size() - 1; i >= 0; i--)
		{
			Operation op = (Operation) log.get(i);
			applyOperation(op.invert(), node, false);
		}

		for (int i = 0; i < node.getNrChildren(); i++)
		{
			undoOperations(node.getChildAt(i));
		}
	}

	/**
	 * Returns a new list of operations which contains the operations in the original <code>log</code>
	 * inversed and in reverse order.
	 */
	private static ArrayList inverse(ArrayList log)
	{
		ArrayList result = new ArrayList();
		for (int i = log.size() - 1; i >= 0; i--)
		{
			Operation op = (Operation) log.get(i);
			result.add(op.invert());
		}

		return result;
	}

	/**
	 * Returns true if all the logs in the subtree which has <code>node</code> as its root (including the
	 * log of <code>node</code>) are empty and false otherwise.
	 */
	private boolean isLogEmpty(DocumentTreeNode node)
	{
		if (!node.getLog().isEmpty())
		{
			return false;
		}

		for (int i = 0; i < node.getNrChildren(); i++)
		{
			if (!isLogEmpty(node.getChildAt(i)))
			{
				return false;
			}
		}

		return true;
	}

	/**
	 * Adds separators between all adjacent units (paragraphs, sentences, words) which exist in the document tree
	 * and are not separated by a separator. This solves some cases in which 2 or more users add text in the exact
	 * position and, although both units appear in the tree, they are not visually separated in the text gui.
	 */
	private void addSeparators()
	{
		addSeparators(root);
	}

	/**
	 * The implementation of addSeparators which does the actual work.
	 *
	 * @param node should be called with the root node
	 */
	private void addSeparators(DocumentTreeNode node)
	{
		boolean modif;
		do
		{
			modif = false;
			for (int i = 0; i < node.getNrChildren() - 1; i++)
			{
				if (!node.getChildAt(i).isSeparator() && !node.getChildAt(i+1).isSeparator())
				{
					int[] indices = node.getChildAt(i+1).getIndices();
					Operation op = new Operation(OperationType.insert, indices, "" +
					        Parser.getDefaultSeparator(node.getLevel()));
					applyOperation(op, true);
					modif = true;
					break;
				}
			}
		}
		while (modif);

		for (int i = 0; i < node.getNrChildren(); i++)
		{
			addSeparators(node.getChildAt(i));
		}
	}

	/**
	 * Traverses the entire document tree and locks operations at all levels.
	 */
	private void lockAll()
	{
		lockAll(root);
	}

	/**
	 * The implementation of lockAll which does the actual work.
	 *
	 * @param node should be called with the root node
	 */
	private void lockAll(DocumentTreeNode node)
	{
		for (int i = 0; i < node.getNrChildren(); i++)
		{
			lockAll(node.getChildAt(i));
		}

		ArrayList log = node.getLog();
		for (int i = 0; i < log.size(); i++)
		{
			((Operation) log.get(i)).setLocked(true);
		}
	}

	/**
	 * The private version of emptyLog. Does the actual work.
	 *
	 * @param node should be called with the root node
	 */
	private void emptyLog(DocumentTreeNode node)
	{
		node.getLog().clear();
		for (int i = 0; i < node.getNrChildren(); i++)
		{
			emptyLog(node.getChildAt(i));
		}
	}

	/**
	 * the root of the document tree
	 */
	private DocumentTreeNode root;

	/**
	 * the current version of reference of this document
	 */
	private int currentVersion;

	/**
	 * The frame in which this document is displayed. This is very ugly (!), but is currently need for displaying
	 * a choice to the user when a merge decision has to be made.
	 */
	private Frame parentFrame;

	//private JTextPane textPane;

	/**
	 * internal flag used in order to temporarily disable notification of observers
	 */
	private boolean disableObservers;
}