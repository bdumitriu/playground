package editor;

import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.AttributeSet;

/**
 * Implementation of the (PDK)Element interface designed to simply hold
 * single tokens. The tokens aren't actually kept here as strings. Only
 * their start and end offset is retained.
 * <br /><br />
 * A very important method is getType(), which will return one of the types
 * in editor.Constants, providing information regarding the specific type of
 * token represented by the element.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Feb 25, 2003
 */
public class PDKToken implements PDKElement
{
	/* the parent of this element */
	private Element parent;

	/* the name of this element (token) */
	private String name;

	/* the type of this element (token) */
	private int type;

	/* the start and end offset of this element */
	private int startOffset;
	private int endOffset;


	public PDKToken(Element parent, String name, int type, int startOffset, int endOffset)
	{
		this.parent = parent;
		this.name = name;
		this.type = type;
		this.startOffset = startOffset;
		this.endOffset = endOffset;
	}

	public int getType()
	{
		return type;
	}

	public Document getDocument()
	{
		return parent.getDocument();
	}

	public Element getParentElement()
	{
		return parent;
	}

	public String getName()
	{
		return name;
	}

	public AttributeSet getAttributes()
	{
		return null;
	}

	public int getStartOffset()
	{
		return startOffset;
	}

	public int getEndOffset()
	{
		return endOffset;
	}

	public int getElementIndex(int offset)
	{
		return -1;
	}

	public int getElementCount()
	{
		return 0;
	}

	public Element getElement(int index)
	{
		return null;
	}

	public boolean isLeaf()
	{
		return true;
	}

	public String toString()
	{
		StringBuffer buf = new StringBuffer("[token: ");
		buf.append(name);
		buf.append(", type ");
		buf.append(type);
		buf.append(", {");
		buf.append(startOffset);
		buf.append(",");
		buf.append(endOffset);
		buf.append("}]");

		return buf.toString();
	}
}
