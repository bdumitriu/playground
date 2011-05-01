package editor.parser;

/**
 *
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@bdumitriu.ro
 * @version 0.1
 * 
 * Date: Mar 17, 2003
 */
public class PDKChange
{
	private int firstChangedToken;

	private int lastChangedToken;

	public PDKChange(int firstChangedToken, int lastChangedToken)
	{
		this.firstChangedToken = firstChangedToken;
		this.lastChangedToken = lastChangedToken;
	}

	public int getFirstChangedToken()
	{
		return firstChangedToken;
	}

	public int getLastChangedToken()
	{
		return lastChangedToken;
	}

	public void setFirstChangedToken(int firstChangedToken)
	{
		this.firstChangedToken = firstChangedToken;
	}

	public void setLastChangedToken(int lastChangedToken)
	{
		this.lastChangedToken = lastChangedToken;
	}
}
