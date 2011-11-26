package client;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class Friend implements Comparable
{
	public Friend(String name)
	{
		this(name, "", FriendStatus.OFFLINE);
	}

	public Friend(String name, FriendStatus status)
	{
		this(name, "", status);
	}

	public Friend(String name, String statusMessage, FriendStatus status)
	{
		this.name = name;
		this.statusMessage = statusMessage;
		this.status = status;
	}

	public String getName()
	{
		return name;
	}

	public String getStatusMessage()
	{
		return statusMessage;
	}

	public void setStatusMessage(String statusMessage)
	{
		this.statusMessage = statusMessage;
	}

	public FriendStatus getStatus()
	{
		return status;
	}

	public void setStatus(FriendStatus status)
	{
		this.status = status;
	}

	public int compareTo(Object o)
	{
		if (o.getClass() != Friend.class)
		{
			throw new ClassCastException();
		}
		else
		{
			Friend f = (Friend) o;
			return name.compareTo(f.name);
		}
	}

	private String name;
	private String statusMessage;
	private FriendStatus status;
}
