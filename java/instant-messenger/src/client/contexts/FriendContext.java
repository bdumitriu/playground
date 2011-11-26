package client.contexts;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class FriendContext
{
	public FriendContext(Outcome outcome, String userName, String friendName)
	{
		this.outcome = outcome;
		this.userName = userName;
		this.friendName = friendName;
	}

	public Outcome getOutcome()
	{
		return outcome;
	}

	public String getUserName()
	{
		return userName;
	}

	public String getFriendName()
	{
		return friendName;
	}

	private Outcome outcome;
	private String userName;
	private String friendName;
}
