package client.contexts;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class MessageContext
{
	public MessageContext(MessageOutcome outcome, String fromUser, String toUser, String message)
	{
		this.outcome = outcome;
		this.fromUser = fromUser;
		this.toUser = toUser;
		this.message = message;
	}

	public MessageOutcome getOutcome()
	{
		return outcome;
	}

	public String getFromUser()
	{
		return fromUser;
	}

	public void setFromUser(String fromUser)
	{
		this.fromUser = fromUser;
	}

	public String getToUser()
	{
		return toUser;
	}

	public void setToUser(String toUser)
	{
		this.toUser = toUser;
	}

	public String getMessage()
	{
		return message;
	}

	public void setMessage(String message)
	{
		this.message = message;
	}

	private MessageOutcome outcome;
	private String fromUser;
	private String toUser;
	private String message;
}
