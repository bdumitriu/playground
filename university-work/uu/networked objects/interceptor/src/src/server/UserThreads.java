package server;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class UserThreads
{
	public UserThreads(String userName)
	{
		this.userName = userName;
		this.inThread = null;
		this.outThread = null;
	}

	public UserThreads(String userName, InThread inThread, OutThread outThread)
	{
		this.userName = userName;
		this.inThread = inThread;
		this.outThread = outThread;
	}

	public String getUserName()
	{
		return userName;
	}

	public InThread getInThread()
	{
		return inThread;
	}

	public void setInThread(InThread inThread)
	{
		this.inThread = inThread;
	}

	public OutThread getOutThread()
	{
		return outThread;
	}

	public void setOutThread(OutThread outThread)
	{
		this.outThread = outThread;
	}

	private String userName;
	private InThread inThread;
	private OutThread outThread;
}
