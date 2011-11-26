package client.contexts;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 19, 2005
 */
public class LoginContext
{
	public LoginContext(Outcome outcome, String userName)
	{
		this.outcome = outcome;
		this.userName = userName;
	}

	public Outcome getOutcome()
	{
		return outcome;
	}

	public String getUserName()
	{
		return userName;
	}

	private Outcome outcome;
	private String userName;
}
