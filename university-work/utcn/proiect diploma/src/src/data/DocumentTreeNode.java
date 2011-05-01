package data;

import enum.OperationType;
import enum.TreeLevel;
import java.util.*;
import core.Operation;

/**
 * This class represents a node of the document tree. It encloses information like node level, node content, etc.
 */
public class DocumentTreeNode implements Cloneable
{
	/**
	 * Creates a new document level node with null content and no children.
	 */
	public DocumentTreeNode()
	{
		level = TreeLevel.document;
		length = 0;
		content = null;
		children = new ArrayList();
		log = new ArrayList();
		separator = false;
		parent = null;
		childNo = -1;
	}

	public int getLevel()
	{
		return level;
	}

	public void setLevel(int level)
	{
		this.level = level;
	}

	public int getLength()
	{
		return length;
	}

	public String getContent()
	{
		return content;
	}

	public void setContent(String content)
	{
		int delta = content.length();
		if (this.content != null)
		{
			delta -= this.content.length();
		}

		this.content = content;

		// update the length attribute of all the ancestors of this node
		DocumentTreeNode w = this;
		while (w != null)
		{
			w.length += delta;
			w = w.getParent();
		}
	}

	public boolean isSeparator()
	{
		return separator;
	}

	public void setSeparator(boolean separator)
	{
		this.separator = separator;
	}

	/**
	 * Returns the number of children of this node.
	 */
	public int getNrChildren()
	{
		return children.size();
	}

	/**
	 * Adds child <code>child</code> to this node on position <code>k</code>.
	 */
	public void addChildAt(int k, DocumentTreeNode child)
	{
		children.add(k, child);
		child.setParent(this);

		int childLength = child.getLength();

		// update the childNo attribute of all the children of this node
		for (int i = k; i < getNrChildren(); i++)
		{
			getChildAt(i).setChildNo(i);
		}

		// update the length attribute of all ancestors of this node
		DocumentTreeNode w = this;
		while (w != null)
		{
			w.length += childLength;
			w = w.getParent();
		}
	}

	/**
	 * Returns the child on position <code>k</code>.
	 */
	public DocumentTreeNode getChildAt(int k)
	{
		return (DocumentTreeNode) children.get(k);
	}

	/**
	 * Sets the child on position <code>k</code> to <code>child</code>.
	 */
	public void setChildAt(int k, DocumentTreeNode child)
	{
		int delta = child.getLength() - getChildAt(k).getLength();

		child.setParent(this);

		children.set(k, child);

		child.setChildNo(k);

		// update the length attribute of all the ancestors of this node
		DocumentTreeNode w = this;
		while (w != null)
		{
			w.length += delta;
			w = w.getParent();
		}
	}

	/**
	 * Removes the child on position <code>k</code>.
	 */
	public void removeChild(int k)
	{
		int childLenght = getChildAt(k).getLength();

		children.remove(k);

		// update the childNo attribute of all the children of this node
		for (int i = k; i < getNrChildren(); i++)
		{
			getChildAt(i).setChildNo(i);
		}

		// update the length attribute of all ancestors of this node
		DocumentTreeNode w = this;
		while (w != null)
		{
			w.length -= childLenght;
			w = w.getParent();
		}
	}

	public DocumentTreeNode getParent()
	{
		return parent;
	}

	public void setParent(DocumentTreeNode parent)
	{
		this.parent = parent;
	}

	/**
	 * Returns the number which indicates the position of this node among its parents children.
	 */
	public int getChildNo()
	{
		return childNo;
	}

	private void setChildNo(int childNo)
	{
		this.childNo = childNo;
	}

	public ArrayList getLog()
	{
		return log;
	}

	/**
	 * Adds <code>op</code> to the log of this node.
	 */
	public void addOperationToLog(Operation op)
	{
		if (op.getType() != OperationType.nop)
		{
			log.add(op);
		}
	}

	/**
	 * Deletes all operations from the log of this node.
	 */
	public void emptyLog()
	{
		log.clear();
	}

	/**
	 * Returns the indices which have to be followed from the root of the tree in order to reach this node.
	 */
	public int[] getIndices()
	{
		int[] indices;
		switch (level)
		{
			case TreeLevel.document:
				indices = null;
				break;
			case TreeLevel.paragraph:
				indices = new int[1];
				indices[0] = getChildNo();
				break;
			case TreeLevel.sentence:
				indices = new int[2];
				indices[0] = getParent().getChildNo();
				indices[1] = getChildNo();
				break;
			case TreeLevel.word:
				indices = new int[3];
				indices[0] = getParent().getParent().getChildNo();
				indices[1] = getParent().getChildNo();
				indices[2] = getChildNo();
				break;
			default:
				indices = null;
				break;
		};

		return indices;
	}

	public String toString()
	{
		if (content != null)
		{
			return content;
		}

		String result = "";
		for (int i = 0; i < getNrChildren(); i++)
		{
			result += getChildAt(i);
		}

		return result;
	}

	public Object clone()
	{
		DocumentTreeNode dolly;
		try
		{
			dolly = (DocumentTreeNode) super.clone();

			dolly.children = (ArrayList) dolly.children.clone();
			for (int i = 0; i < dolly.getNrChildren(); i++)
			{
				dolly.children.set(i, dolly.getChildAt(i).clone());
				DocumentTreeNode child = (DocumentTreeNode)dolly.children.get(i);
				child.setParent(dolly);
			}

			dolly.log = (ArrayList) dolly.log.clone();
			for (int i = 0; i < dolly.log.size() ; i++)
			{
				dolly.log.set(i, ((Operation) dolly.log.get(i)).clone());
			}

		}
		catch (CloneNotSupportedException e)
		{
			dolly = null;
			e.printStackTrace();
		}

		return dolly;
	}

	/**
	 * the node level: document, paragraph, sentence or word
	 */
	private int level;

	/**
	 * the number of characters this node represents
	 */
	private int length;

	/**
	 * for leaves and nodes representing separators it contains the actual content; for all other nodes,
	 * it is empty (since the content is generated automatically based on the values of the children)
	 */
	private String content;

	/**
	 * true if node represents a separator, false otherwise
	 */
	private boolean separator;

	/**
	 * the list of children of this node
	 */
	private ArrayList children;

	/**
	 * the parent of this node
	 */
	private DocumentTreeNode parent;

	/**
	 * this number represents the position of this node among the children of its parent
	 */
	private int childNo;

	/**
	 * the log of operations which affected this node
	 */
	private ArrayList log;
}