package geditor.users;

/**
 * Container class for ip - user mapping.
 * <br /><br />
 * Date: Mar 8, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class EditorUser
{
	public EditorUser()
	{
		this.ip = "";
		this.username = "";
	}

	public EditorUser(EditorUser editoruser)
	{
		this.ip = editoruser.ip;
		this.username = editoruser.username;
	}

	public EditorUser(String ip, String username)
	{
		this.ip = ip;
		this.username = username;
	}

	public String getIp()
	{
		return ip;
	}

	public void setIp(String ip)
	{
		this.ip = ip;
	}

	public String getUsername()
	{
		return username;
	}

	public void setUsername(String username)
	{
		this.username = username;
	}

	public boolean equals(Object editorUser)
	{
		if (!(editorUser instanceof EditorUser))
		{
			return false;
		}

		EditorUser other = (EditorUser) editorUser;

		if (other.getIp().equals(getIp()) && (other.getUsername().equals(getUsername())))
		{
			return true;
		}
		else
		{
			return false;
		}
	}

	public String toString()
	{
		return username + " (" + ip + ")";
	}

	private String ip;
	private String username;
}
