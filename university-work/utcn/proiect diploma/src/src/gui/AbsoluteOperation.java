package gui;

import enum.OperationType;

/**
 * This class contains all the information about an operation that is required in order to identify the exact
 * text it inserted/deleted and its absolute position in the document. Aside from this, it also contains
 * information about the site which generated the operation.
 */
public class AbsoluteOperation
{
	/**
	 * Creates a new AbsoluteOperation with default values for all its fields.
	 */
	public AbsoluteOperation()
	{
		this(-1, -1, "", "");
	}

	public AbsoluteOperation(int type, int position, String content, String sourceIp)
	{
		this.type = type;
		this.position = position;
		this.content = content;
		this.sourceIp = sourceIp;
	}

	public int getType()
	{
		return type;
	}

	public void setType(int type)
	{
		this.type = type;
	}

	public int getPosition()
	{
		return position;
	}

	public void setPosition(int position)
	{
		this.position = position;
	}

	public String getContent()
	{
		return content;
	}

	public void setContent(String content)
	{
		this.content = content;
	}

	public String getSourceIp()
	{
		return sourceIp;
	}

	public void setSourceIp(String sourceIp)
	{
		this.sourceIp = sourceIp;
	}

	public int getSize()
	{
		return content.length();
	}

	public String toString()
	{
		if (type == OperationType.insert)
		{
			return "(insert " + position + ", " + content + ", " + content.length() + ")";
		}
		else if (type == OperationType.delete)
		{
			return "(delete " + position + ", " + content + ", " + content.length() + ")";
		}
		else if (type == OperationType.nop)
		{
			return "(nop)";
		}
		else
		{
			return "";
		}
	}

	private int type;
	private int position;
	private String content;
	private String sourceIp;
}
