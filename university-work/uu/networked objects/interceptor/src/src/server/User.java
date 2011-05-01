package server;

import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.io.Serializable;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.ObjectInputStream;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class User implements Serializable
{
	public User(String name)
	{
		this.name = name;
		this.status = "";
		this.friends = new HashSet<String>();
	}

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public String getPassword()
	{
		return password;
	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	public String getStatus()
	{
		return status;
	}

	public void setStatus(String status)
	{
		this.status = status;
	}

	public void addFriend(String friend)
	{
		friends.add(friend);
	}

	public void removeFriend(String friend)
	{
		friends.remove(friend);
	}

	public Set<String> getFriends()
	{
		return friends;
	}

	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}

		final User user = (User) o;

		if (name != null ? !name.equals(user.name) : user.name != null)
		{
			return false;
		}

		return true;
	}

	public int hashCode()
	{
		return (name != null ? name.hashCode() : 0);
	}

	private String name;
	private String password;
	private String status;
	private Set<String> friends;
}
