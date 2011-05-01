package core;

import java.io.Serializable;

/**
 * Created by IntelliJ IDEA.
 * User: Lau
 * Date: Mar 13, 2005
 * Time: 8:04:03 AM
 */
public class UserData implements Serializable
{
	public UserData()
	{
	}

	public UserData(String loginName, String name)
	{
		this.loginName = loginName;
		this.name = name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return this.name;
	}

	public void setLoginName(String loginName)
	{
		this.loginName = loginName;
	}

	public String getLoginName()
	{
		return loginName;
	}

	private String name;
	private String loginName;
}


