package core;

/**
 * This class represents a text gui opertion (such as InsertWord, DeleteSentence, etc.). Operations are identified
 * by a global unique identifier of the form ip%sessionId%operationId. An operation is defined by its type (insert
 * or delete), the level to which it refers (document, paragraph, sentence or word), its text content and its
 * indices (the number of which depends on the level of the operation).
 * <br /><br />
 * The Operation class also has support for serialization/deserialization in XML form.
 */

import org.w3c.dom.*;
import org.w3c.dom.Document;

import java.io.*;
import java.util.ArrayList;
import java.rmi.server.UID;
import java.net.InetAddress;
import java.net.UnknownHostException;

import enum.OperationType;
import enum.TreeLevel;

public class Operation implements Cloneable, Serializable
{
	static
	{
		try
		{
			localIp = InetAddress.getLocalHost().getHostAddress();
			sessionId = (new UID()).toString();
		}
		catch (UnknownHostException e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * Creates a new Operation of type nop, document level.
	 */
	public Operation()
	{
		this.locked = false;
		this.level = TreeLevel.document;
		this.type = OperationType.nop;
		this.guid = "";
		this.index = new int[4];
		this.index[0] = 0;
		this.index[1] = 0;
		this.index[2] = 0;
		this.index[3] = 0;
		this.content = "nop";
		generateGUID();
	}

	/**
	 * Creates a new document level operation.
	 *
	 * @param type the operation type
	 * @param pIdx the paragraph index
	 * @param content the content of the operation
	 */
	public Operation(int type, int pIdx, String content)
	{
		this();

		this.type = type;
		this.level = TreeLevel.document;
		this.index[0] = pIdx;
		this.content = content;
	}

	/**
	 * Creates a new paragraph level operation.
	 *
	 * @param type the operation type
	 * @param pIdx the paragraph index
	 * @param sIdx the sentence index
	 * @param content the content of the operation
	 */
	public Operation(int type, int pIdx, int sIdx, String content)
	{
		this();

		this.type = type;
		this.level = TreeLevel.paragraph;
		this.index[0] = pIdx;
		this.index[1] = sIdx;
		this.content = content;
	}

	/**
	 * Creates a new sentence level operation.
	 *
	 * @param type the operation type
	 * @param pIdx the paragraph index
	 * @param sIdx the sentence index
	 * @param wIdx the word index
	 * @param content the content of the operation
	 */
	public Operation(int type, int pIdx, int sIdx, int wIdx, String content)
	{
		this();

		this.type = type;
		this.level = TreeLevel.sentence;
		this.index[0] = pIdx;
		this.index[1] = sIdx;
		this.index[2] = wIdx;
		this.content = content;
	}

	/**
	 * Creates a new word level operation.
	 *
	 * @param type the operation type
	 * @param pIdx the paragraph index
	 * @param sIdx the sentence index
	 * @param wIdx the word index
	 * @param cIdx the character index
	 * @param content the content of the operation
	 */
	public Operation(int type, int pIdx, int sIdx, int wIdx, int cIdx, String content)
	{
		this();

		this.type = type;
		this.level = TreeLevel.word;
		this.index[0] = pIdx;
		this.index[1] = sIdx;
		this.index[2] = wIdx;
		this.index[3] = cIdx;
		this.content = content;
	}

	/**
	 * Creates a new operation whose level is specified by the <code>index</code>'s length.
	 *
	 * @param type the type of the operation
	 * @param index the indices of the operation
	 * @param content the content of the operation
	 */
	public Operation(int type, ArrayList index, String content)
	{
		this();

		this.type = type;
		switch (index.size())
		{
			case 1:
				this.level = TreeLevel.document;
				break;
			case 2:
				this.level = TreeLevel.paragraph;
				break;
			case 3:
				this.level = TreeLevel.sentence;
				break;
			case 4:
				this.level = TreeLevel.word;
				break;
		}
		this.content = content;
		for (int i = 0; i < index.size(); i++)
		{
			this.index[i] = ((Integer) index.get(i)).intValue();
		}
	}

	/**
	 * Creates a new operation whose level is specified by the <code>index</code>'s length.
	 *
	 * @param type the type of the operation
	 * @param index the indices of the operation
	 * @param content the content of the operation
	 */
	public Operation(int type, int[] index, String content)
	{
		this();

		this.type = type;
		switch (index.length)
		{
			case 1:
				this.level = TreeLevel.document;
				break;
			case 2:
				this.level = TreeLevel.paragraph;
				break;
			case 3:
				this.level = TreeLevel.sentence;
				break;
			case 4:
				this.level = TreeLevel.word;
				break;
		}
		this.content = content;
		this.index = (int[]) index.clone();
	}

	public int getType()
	{
		return type;
	}

	public void setType(int type)
	{
		this.type = type;
	}

	public int getLevel()
	{
		return level;
	}

	public String getGuid()
	{
		return guid;
	}

	public boolean isLocked()
	{
		return locked;
	}

	public void setLocked(boolean locked)
	{
		this.locked = locked;
	}

	public String getContent()
	{
		return content;
	}

	public void setContent(String content)
	{
		this.content = content;
	}

	public int[] getIndex()
	{
		return index;
	}

	/**
	 * Sets the index from an ArrayList. If the size of the ArrayList is not compatible with the the level of this
	 * operation no changes are made.
	 *
	 * @param index the list of indices
	 * @return true if changes were made (i.e. size of ArrayList was compatible with operation type) and false
	 *	otherwise
	 */
	public boolean setIndex(ArrayList index)
	{
		int indexSize = index.size();
		switch (level)
		{
			case TreeLevel.document:
				if (indexSize != 1)
				{
					return false;
				}
				else
				{
					this.index[0] = ((Integer) index.get(0)).intValue();
				}
				break;
			case TreeLevel.paragraph:
				if (indexSize != 2)
				{
					return false;
				}
				else
				{
					this.index[0] = ((Integer) index.get(0)).intValue();
					this.index[1] = ((Integer) index.get(1)).intValue();
				}
				break;
			case TreeLevel.sentence:
				if (indexSize != 3)
				{
					return false;
				}
				else
				{
					this.index[0] = ((Integer) index.get(0)).intValue();
					this.index[1] = ((Integer) index.get(1)).intValue();
					this.index[2] = ((Integer) index.get(2)).intValue();
				}
				break;
			case TreeLevel.word:
				if (indexSize != 4)
				{
					return false;
				}
				else
				{
					this.index[0] = ((Integer) index.get(0)).intValue();
					this.index[1] = ((Integer) index.get(1)).intValue();
					this.index[2] = ((Integer) index.get(2)).intValue();
					this.index[3] = ((Integer) index.get(3)).intValue();
				}
				break;
			default:
				return false;
		}

		return true;
	}

	/**
	 * Sets the paragraph index to <code>value</code>.
	 */
	public void setPIdx(int value)
	{
		index[0] = value;
	}

	/**
	 * Sets the sentence index to <code>value</code>.
	 *
	 * @return false if operation does not need a sentence index, true otherwise.
	 */
	public boolean setSIdx(int value)
	{
		if (level != TreeLevel.document)
		{
			index[1] = value;
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Sets the word index to <code>value</code>.
	 *
	 * @return false if operation does not need a word index, true otherwise.
	 */
	public boolean setWIdx(int value)
	{
		if ((level != TreeLevel.document) && (level != TreeLevel.paragraph))
		{
			index[2] = value;
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Sets the character index to <code>value</code>.
	 *
	 * @return false if operation does not need a character index, true otherwise.
	 */
	public boolean setCIdx(int value)
	{
		if (level == TreeLevel.word)
		{
			index[3] = value;
			return true;
		}
		else
		{
			return false;
		}
	}

	/**
	 * Returns the index that corresponds to the level of this operation (i.e. the word index for insert/delete
	 * word operations, the character index for insert/delete character operations, etc.)
	 *
	 * @return the index that corresponds to the level of this operation
	 */
	public int getLevelIndex()
	{
		return index[level];
	}

	/**
	 * Returns 1 for document level operations, 2 for paragraph level operations, 3 for sentence level
	 * operations and 4 for word level operations.
	 */
	public int getLevelNumber()
	{
		switch (level)
		{
			case TreeLevel.document:
				return 1;
			case TreeLevel.paragraph:
				return 2;
			case TreeLevel.sentence:
				return 3;
			case TreeLevel.word:
				return 4;
			default:
				return -1;
		}
	}

	/**
	 * Returns the ip part of the site on which this operation was generated.
	 */
	public String getIp()
	{
		return guid.substring(0, guid.indexOf('%'));
	}

	/**
	 * Returns the site on which this operation was generated.
	 */
	public String getSite()
	{
		return guid.substring(0, guid.lastIndexOf('%'));
	}

	/**
	 * Returns the operation obtained by excluding the effect of <code>opAgainst</code> from the
	 * effect of this operation.
	 *
	 * @param opAgainst the operation whose effect to exclude
	 */
	public Operation exclude(Operation opAgainst)
	{
		Operation result;
		result = (Operation) clone();

		if (opAgainst.getType() == OperationType.nop ||	getType() == OperationType.nop)
		{
			return result;
		}

		int levelCurrent = getLevelNumber();;
		int levelAgainst = opAgainst.getLevelNumber();

		if (levelAgainst > levelCurrent)
		{
			return result;
		}

		for (int i = 0; i < levelAgainst - 1; i++)
		{
			if (this.index[i] != opAgainst.index[i])
			{
				return result;
			}
		}

		int k = levelAgainst - 1;
		if (opAgainst.getType() == OperationType.insert)
		{
			if (this.index[k] == opAgainst.index[k])
			{
				if (levelCurrent != levelAgainst ||
					(levelCurrent == levelAgainst && this.getType() == OperationType.delete))
				{
					result.setType(OperationType.nop);
				}
			}
			if (this.index[k] > opAgainst.index[k])
			{
				result.index[k]--;
			}
		}
		else if (opAgainst.getType() == OperationType.delete)
		{
			if (this.index[k] >= opAgainst.index[k])
			{
				result.index[k]++;
			}
		}

		return result;
	}

	/**
	 * Returns the operation obtained by including the effect of <code>opAgainst</code> in the
	 * effect of this operation.
	 *
	 * @param opAgainst the operation whose effect to include
	 */
	public Operation include(Operation opAgainst)
	{
		Operation result;
		result = (Operation) clone();

		if (opAgainst.getType() == OperationType.nop || getType() == OperationType.nop)
		{
			return result;
		}

		int levelCurrent = getLevelNumber();
		int levelAgainst = opAgainst.getLevelNumber();

		if (levelAgainst > levelCurrent)
		{
			return result;
		}

		for (int i = 0; i < levelAgainst - 1; i++)
		{
			if (this.index[i] != opAgainst.index[i])
			{
				return result;
			}
		}

		int k = levelAgainst - 1;
		if (opAgainst.getType() == OperationType.delete)
		{
			if (this.index[k] == opAgainst.index[k])
			{
				if (levelCurrent != levelAgainst ||
					(levelCurrent == levelAgainst && this.getType() == OperationType.delete))
				{
					result.setType(OperationType.nop);
				}
			}
			if (this.index[k] > opAgainst.index[k])
			{
				result.index[k]--;
			}
		}
		else if (opAgainst.getType() == OperationType.insert)
		{
			if (this.getType() == OperationType.insert && this.index[k] == opAgainst.index[k] &&
				levelCurrent == levelAgainst)
			{
				if (!this.getSite().equals(opAgainst.getSite()))
				{
					if (this.getContent().compareTo(opAgainst.getContent()) <= 0)
					{
						result.index[k]++;
					}
				}

				if (this.getGuid().equals(opAgainst.getGuid()))
				{
					result.setType(OperationType.nop);
				}
			}
			else
			{
				if (this.index[k] >= opAgainst.index[k])
				{
					result.index[k]++;
				}
			}
		}

		return result;
	}

	/**
	 * Returns the operation that reverses the effect of this operation.
	 */
	public Operation invert()
	{
		Operation r = (Operation) clone();

		if (getType() == OperationType.insert)
		{
			r.setType(OperationType.delete);
		}

		if (getType() == OperationType.delete)
		{
			r.setType(OperationType.insert);
		}

		r.generateGUID();

		return r;
	}

	public Object clone()
	{
		try
		{
			Operation newOp = (Operation) super.clone();
			newOp.index = (int[]) index.clone();
			return newOp;
		}
		catch (CloneNotSupportedException e)
		{
			return null;
		}
	}

	public String toString()
	{
		if (type == OperationType.nop)
		{
			return "nop";
		}

		String result;

		if (type == OperationType.insert)
		{
			result = "insert";
		}
		else if (type == OperationType.delete)
		{
			result = "delete";
		}
		else
		{
			result = "";
		}

		switch (level)
		{
			case TreeLevel.document:
				result += " paragraph " + "(" + index[0] + ") ";
				break;
			case TreeLevel.paragraph:
				result += " sentence " + "(" + index[0] + "," + index[1] + ") ";
				break;
			case TreeLevel.sentence:
				result += " word " + "(" + index[0] + "," + index[1] + "," + index[2] + ") ";
				break;
			case TreeLevel.word:
				result += " char " + "(" + index[0] + "," + index[1] + "," + index[2] + "," + index[3] + ") ";
		}

		result += "\"" + content + "\"";

		return result;
	}

	/**
	 * Creates a representation of this Operation as an Element node of a DOM Document and returns it.
	 *
	 * @param doc the Document to use in order to generate instances of Node's
	 * @return the generated Element.
	 */
	public Node toDocumentElement(Document doc)
	{
		Element root = doc.createElement("operation");

		root.setAttribute("type", (new Integer(type)).toString());
		root.setAttribute("level", (new Integer(level)).toString());

		Element temp;

		temp = doc.createElement("guid");
		temp.appendChild(doc.createTextNode(guid));
		root.appendChild(temp);

		temp = doc.createElement("pIdx");
		temp.appendChild(doc.createTextNode((new Integer(index[0])).toString()));
		root.appendChild(temp);

		if (level != TreeLevel.document)
		{
			temp = doc.createElement("sIdx");
			temp.appendChild(doc.createTextNode((new Integer(index[1])).toString()));
			root.appendChild(temp);
		}

		if ((level == TreeLevel.sentence) || (level == TreeLevel.word))
		{
			temp = doc.createElement("wIdx");
			temp.appendChild(doc.createTextNode((new Integer(index[2])).toString()));
			root.appendChild(temp);
		}

		if (level == TreeLevel.word)
		{
			temp = doc.createElement("cIdx");
			temp.appendChild(doc.createTextNode((new Integer(index[3])).toString()));
			root.appendChild(temp);
		}

		temp = doc.createElement("content");
		temp.appendChild(doc.createTextNode(content));
		root.appendChild(temp);

		return root;
	}

	/**
	 * Modifies the members of this Operation with the values read from an element Node of a DOM Document. The
	 * element node has to be the one containing the "operation" tag.
	 * <br /><br />
	 * @param root the root node of the subtree containig data about this operation
	 * @return true if all went ok, false if data members could not be properly initialized. Beware that some of
	 *	the values might have changed while some might not if false is returned.
	 */
	public boolean fromDocumentElement(Node root)
	{
		NamedNodeMap attributes = root.getAttributes();

		try
		{
			type = new Integer(attributes.getNamedItem("type").getNodeValue()).intValue();
			level = new Integer(attributes.getNamedItem("level").getNodeValue()).intValue();
		}
		catch (NumberFormatException e)
		{
			e.printStackTrace();
			return false;
		}
		catch (DOMException e)
		{
			e.printStackTrace();
			return false;
		}

		NodeList opData = root.getChildNodes();

		// first try to find out the indices of each of the elements we are interested in
		int dataPosition[] = new int[6];
		for (int i = 0; i < 6; i++)
		{
			dataPosition[i] = -1;
		}
		for (int i = 0; i < opData.getLength(); i++)
		{
			Node currentData = opData.item(i);
			if (currentData.getNodeType() == Node.ELEMENT_NODE)
			{
				String nodeValue = currentData.getNodeName();
				if (nodeValue.equals("guid"))
				{
					dataPosition[0] = i;
				}
				else if (nodeValue.equals("content"))
				{
					dataPosition[1] = i;
				}
				else if (nodeValue.equals("pIdx"))
				{
					dataPosition[2] = i;
				}
				else if (nodeValue.equals("sIdx"))
				{
					dataPosition[3] = i;
				}
				else if (nodeValue.equals("wIdx"))
				{
					dataPosition[4] = i;
				}
				else if (nodeValue.equals("cIdx"))
				{
					dataPosition[5] = i;
				}
			}
		}

		// first check that all required elements (i.e., the first 3) exist
		for (int i = 0; i < 3; i++)
		{
			if (dataPosition[i] == -1)
			{
				return false;
			}
		}

		// now that we know the positions, we can extract the values
		try
		{
			String tmp;

			// extract the guid
			guid  = opData.item(dataPosition[0]).getFirstChild().getNodeValue();

			// extract the content
			content = opData.item(dataPosition[1]).getFirstChild().getNodeValue();

			// extract pIdx
			tmp = opData.item(dataPosition[2]).getFirstChild().getNodeValue();
			index[0] = new Integer(tmp).intValue();
		}
		catch (NumberFormatException e)
		{
			e.printStackTrace();
			return false;
		}
		catch (DOMException e)
		{
			e.printStackTrace();
			return false;
		}

		if (level != TreeLevel.document)
		{
			// extract sIdx
			if (dataPosition[3] == -1)
			{
				return false;
			}
			else
			{
				try
				{
					String tmp = opData.item(dataPosition[3]).getFirstChild().getNodeValue();
					index[1] = new Integer(tmp).intValue();
				}
				catch (NumberFormatException e)
				{
					e.printStackTrace();
					return false;
				}
				catch (DOMException e)
				{
					e.printStackTrace();
					return false;
				}
			}
		}

		if ((level == TreeLevel.sentence) || (level == TreeLevel.word))
		{
			// extract wIdx
			if (dataPosition[4] == -1)
			{
				return false;
			}
			else
			{
				try
				{
					String tmp = opData.item(dataPosition[4]).getFirstChild().getNodeValue();
					index[2] = new Integer(tmp).intValue();
				}
				catch (NumberFormatException e)
				{
					e.printStackTrace();
					return false;
				}
				catch (DOMException e)
				{
					e.printStackTrace();
					return false;
				}
			}
		}

		if (level == TreeLevel.word)
		{
			// extract cIdx
			if (dataPosition[5] == -1)
			{
				return false;
			}
			else
			{
				try
				{
					String tmp = opData.item(dataPosition[5]).getFirstChild().getNodeValue();
					index[3] = new Integer(tmp).intValue();
				}
				catch (NumberFormatException e)
				{
					e.printStackTrace();
					return false;
				}
				catch (DOMException e)
				{
					e.printStackTrace();
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Transforms <code>op1</code> and <code>op2</code> so that the effect of applying the new
	 * <code>op1</code> after the new <code>op2</code> is the same as the effect of applying
	 * the original <code>op2</code> after the  original <code>op1</code>.
	 */
	public static void transpose(Operation op1, Operation op2)
	{
		Operation op;

		op = op2.exclude(op1);

		if (op1.getLevelIndex() == op2.getLevelIndex() &&
		       op1.getType() == OperationType.insert  &&
		       op2.getType() == OperationType.delete)
		{
			op1.setType(OperationType.nop);
			op2.setType(OperationType.nop);
			return;
		}

		if (op1.getLevelIndex() == op2.getLevelIndex() &&
			op1.getType() == OperationType.insert &&
			op2.getType() == OperationType.insert)
		{
			op1.index[op1.getLevel()]++;
			op2.assign(op1);
		}
		else
		{
			if (op1.getLevelIndex() == op.getLevelIndex() &&
				op1.getType() == OperationType.insert &&
				op.getType() == OperationType.insert)
			{
				op2.assign(op1);
			}
			else
			{
				op2.assign(op1.include(op));
			}
		}

		op1.assign(op);
	}

	/**
	 * Makes <code>op1</code> include the effect of <code>op2</code> and <code>op2</code> include the effect
	 * of <code>op1</code>.
	 */
	public static void symInclusion(Operation op1, Operation op2)
	{
		Operation oo1, oo2;
		oo1 = op1.include(op2);
		oo2 = op2.include(op1);

		op1.assign(oo1);
		op2.assign(oo2);
	}

	/**
	 * Generates a new global unique identifier for this operation.
	 */
	private void generateGUID()
	{
		guid = localIp + "%" + sessionId + "%" + new UID();
	}

	/**
	 * Assigns this operation with all the class members of operation <code>op</code>.
	 *
	 * @param op the operation whose class members are assigned to this operation
	 */
	private void assign(Operation op)
	{
		if (op == null)
		{
			return;
		}

		this.type = op.type;
		this.locked = op.locked;
		this.level = op.level;
		this.guid = op.guid;
		this.index = (int[]) op.index.clone();
		this.content = op.content;
	}

	/**
	 * the operation type (insert, delete, nop)
	 */
	private int type;

	/**
	 * the level at which this operation refers (document, paragraph, sentence, word)
	 */
	private int level;

	/**
	 * the text added/deleted by this operation
	 */
	private String content;

	/**
	 * the paragraph, sentence, word and character index
	 */
	private int index[];

	/**
	 * the global unique identifier for this operation
	 */
	private String guid;

	/**
	 * if locked, this operation can not be merged with other operations
	 */
	private boolean locked;

	/**
	 * the IP of the localhost (stored as a static class member for efficiency)
	 */
 	private static String localIp;

	/**
	 * an id to uniquely identify this session
	 */
	private static String sessionId;
}