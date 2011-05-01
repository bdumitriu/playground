package editor;

import javax.swing.text.Element;
import javax.swing.text.Document;
import javax.swing.text.AttributeSet;
import java.util.ArrayList;

/**
 * This class represents the root element of a user-defined tree which stores
 * all the tokens in the Document, as parsed by a custom parser implementation.
 * All these tokens, encapsulated in PDKToken objects, are direct childeren of
 * root. In addition to all childern having root as their parent, root also keeps
 * an ArrayList member containing all its children. This ArrayList is used in
 * order to access all the children.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 *
 * Date: Feb 25, 2003
 */
public class PDKRootElement implements PDKElement
{
	private static final int INITIAL_ARRAY_LIST_CAPACITY = 1000;

	/* the underlying Document */
	private Document doc;

	/* all the childern of this root node are also stored in this ArrayList */
	private ArrayList elements;

	/* the number of elements from the elements ArrayList that are actualy valid */
	private int arraySize;

	public PDKRootElement(Document doc)
	{
		this.doc = doc;
		elements = new ArrayList(INITIAL_ARRAY_LIST_CAPACITY);
		arraySize = 0;
	}

	/**
	 * Adds an element at the end of the <code>elements</code> ArrayList.
	 *
	 * @param elem the element to add.
	 */
	public void add(Element elem)
	{
		if (arraySize < elements.size())
		{
			elements.set(arraySize++, elem);
		}
		else
		{
			elements.add(elem);
			arraySize++;
		}
	}

	/**
	 * Effectively deletes all the elements of the <code>elements</code> ArrayList
	 * starting with the <code>size</code>th element.
	 *
	 * @param newSize the new size of the <code>elements</code> ArrayList.
	 */
	public void setSize(int newSize)
	{
		arraySize = newSize;
	}

	public Document getDocument()
	{
		return doc;
	}

	public Element getParentElement()
	{
		return null;
	}

	public String getName()
	{
		return Constants.ROOT_ELEMENT_NAME;
	}

	public AttributeSet getAttributes()
	{
		return null;
	}

	public int getStartOffset()
	{
		return 0;
	}

	public int getEndOffset()
	{
		if (arraySize == 0)
		{
			return 0;
		}
		else
		{
			return ((PDKToken) elements.get(arraySize - 1)).getEndOffset();
		}
	}

	/*
	 * The following method is implemented in such a manner that it is independent of changes in
	 * the tree organization policy, i.e. it only uses access to private memebers through
	 * appropriate method calls, not directly. That means that if the private members change, as
	 * long as the other methods are updated apropriately, this method can be left unchanged.
	 */

	/**
	 * In addition to the general description given in the interface definition, there are
	 * several things worth mentioning.
	 * <br /><br />
	 * If <code>offset</code> is lower than or equal to 0, a value of 0 is returned if this element
	 * has any childern and a value of -1 is returned if this element doesn't have any childern.
	 * <br /><br />
	 * The policy regarding <code>offset</code>s that belong to two adjoining childern (this will only
	 * happen with <code>offset</code>s that separate the two children) is such that the child having
	 * <code>offset</code> equal to its endOffset is returned.
	 * <br /><br />
	 * If <code>offset</code> doesn't belong to any child, but is in the offset range of this
	 * element, then the index of the child that is closest to it is returned. In case of equality
	 * between two children, the lower index is returned.
	 *
	 * @return the index of the child closest to <code>offset</code>.
	 */
	public int getElementIndex(int offset)
	{
		// offset should never be < 0...
		if (offset <= 0)
		{
			if (arraySize > 0)
				return 0;
			else
				return -1;
		}

		// the following <if> obeys the general contract of the method,
		// as defined by the interface
		if (offset >= getEndOffset())
		{
			return getElementCount() - 1;
		}

		// do a (sort of) binary search
		int inferiorLimit = 0;
		int superiorLimit = getElementCount() - 1;
		int middle =  superiorLimit / 2;

		while (inferiorLimit != superiorLimit)
		{
			Element current = getElement(middle);
			if (current.getStartOffset() < offset)
			{
				if (offset <= current.getEndOffset())
				{
					return middle;
				}
				else
				{
					inferiorLimit = middle + 1;
				}
			}
			else
			{
				superiorLimit = middle - 1;
			}
			middle = (superiorLimit + inferiorLimit) / 2;
		}

		Element current = getElement(middle);
		if (current.getStartOffset() < offset)
		{
			if (offset <= current.getEndOffset())
			{
				return middle;
			}
			else
			{
				// compute which is closer to the offset: element[middle + 1] or element[middle]
				if (middle + 1 > getElementCount())
				{
					// this means we have no element[middle + 1]
					return middle;
				}
				else
				{
					Element next = getElement(middle + 1);
					int delta = next.getStartOffset() - current.getEndOffset();
					offset -= current.getEndOffset();
					if (offset <= delta / 2)
					{
						return middle;
					}
					else
					{
						return middle + 1;
					}
				}
			}
		}
		else
		{
			// compute which is closer to the offset: element[middle - 1] or element[middle]
			if (middle - 1 < 0)
			{
				// this means we have no element[middle - 1]
				return middle;
			}
			else
			{
				Element previous = getElement(middle - 1);
				int delta = current.getStartOffset() - previous.getEndOffset();
				offset -= previous.getEndOffset();
				if (offset <= delta / 2)
				{
					return middle - 1;
				}
				else
				{
					return middle;
				}
			}
		}
	}

	public int getElementCount()
	{
		return arraySize;
	}

	public Element getElement(int index) throws IndexOutOfBoundsException
	{
		if ((index < 0) || (index >= arraySize))
		{
			throw new IndexOutOfBoundsException();
		}

		return (Element) elements.get(index);
	}

	public boolean isLeaf()
	{
		return false;
	}

	public int getType()
	{
		return Constants.ROOT_ELEMENT_TYPE;
	}
}
