package common;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class Message implements Serializable
{
	public Message()
	{
		this(MessageType.OK, null, null);
	}

	public Message(MessageType type)
	{
		this(type, null, null);
	}

	public Message(MessageType type, String value1)
	{
		this(type, value1, null);
	}

	public Message(MessageType type, String value1, String value2)
	{
		this.type = type;
		this.value1 = value1;
		this.value2 = value2;
	}

	public MessageType getType()
	{
		return type;
	}

	public void setType(MessageType type)
	{
		this.type = type;
	}

	public String getValue1()
	{
		return value1;
	}

	public void setValue1(String value1)
	{
		this.value1 = value1;
	}

	public String getValue2()
	{
		return value2;
	}

	public void setValue2(String value2)
	{
		this.value2 = value2;
	}

	public MessageType type;
	public String value1;
	public String value2;
}
