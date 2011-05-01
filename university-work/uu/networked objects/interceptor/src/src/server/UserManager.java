package server;

import client.Friend;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.ArrayList;
import java.io.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class UserManager
{
	public static UserManager getInstance()
	{
		return instance;
	}

	public User getUser(String userName)
	{
		return allUsers.get(userName);
	}

	synchronized public boolean addFriend(String userName, String friendName)
	{
		if (allUsers.containsKey(friendName))
		{
			getUser(userName).addFriend(friendName);
			return true;
		}
		else
		{
			return false;
		}
	}

	synchronized public boolean removeFriend(String userName, String friendName)
	{
		if (allUsers.containsKey(friendName))
		{
			getUser(userName).removeFriend(friendName);
			return true;
		}
		else
		{
			return false;
		}
	}

	synchronized public ArrayList<User> getOnlineFriendsOf(String userName)
	{
		User u = getUser(userName);
		ArrayList<User> result = null;
		if (u != null)
		{
			result = new ArrayList<User>();
			Set<String> friends = u.getFriends();
			for (User user : loggedUsers.values())
			{
				if (friends.contains(user.getName()))
				{
					result.add(user);
				}
			}
		}
		return result;
	}

	public boolean isOnline(String userName)
	{
		return loggedUsers.containsKey(userName);
	}

	synchronized public void setOnline(String userName, boolean isOnline)
	{
		if (!isOnline)
		{
			loggedUsers.remove(userName);
		}
		else
		{
			User user = getUser(userName);
			if (user != null)
			{
				loggedUsers.put(userName, user);
			}
		}
	}


	/**
	 * Saves the users list to "users.dat".
	 *
	 * @return true if save successful, false otherwise.
	 */
	synchronized public boolean saveUsers()
	{
		try
		{
			ObjectOutputStream file = new ObjectOutputStream(new FileOutputStream("users.dat"));
			file.writeObject(allUsers.size());
			for (User user : allUsers.values())
			{
				file.writeObject(user);
			}
			file.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			return false;
		}
		return true;
	}

	private UserManager()
	{
		allUsers = new HashMap<String, User>();
		loggedUsers = new HashMap<String, User>();
		try
		{
			ObjectInputStream file = new ObjectInputStream(new FileInputStream("users.dat"));
			int n = (Integer) file.readObject();
			for (int i = 0; i < n; i++)
			{
				User user = (User) file.readObject();
				allUsers.put(user.getName(), user);
			}
			file.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
/*
		User u;

		u = new User("bdumitriu");
		u.setPassword("bdumitriu");
		allUsers.put(u.getName(), u);

		u = new User("dana");
		u.setPassword("dana");
		allUsers.put(u.getName(), u);

		u = new User("joe");
		u.setPassword("joe");
		allUsers.put(u.getName(), u);

		u = new User("andy");
		u.setPassword("andy");
		allUsers.put(u.getName(), u);

		u = new User("steve");
		u.setPassword("steve");
		allUsers.put(u.getName(), u);

		u = new User("eric");
		u.setPassword("eric");
		allUsers.put(u.getName(), u);

		u = new User("tom");
		u.setPassword("tom");
		allUsers.put(u.getName(), u);

		u = new User("bill");
		u.setPassword("bill");
		allUsers.put(u.getName(), u);

		u = new User("Mr. Smith");
		u.setPassword("Mr. Smith");
		allUsers.put(u.getName(), u);

		u = new User("george");
		u.setPassword("george");
		allUsers.put(u.getName(), u);

		u = new User("sue");
		u.setPassword("sue");
		allUsers.put(u.getName(), u);

		u = new User("jill");
		u.setPassword("jill");
		allUsers.put(u.getName(), u);

		u = new User("kathy");
		u.setPassword("kathy");
		allUsers.put(u.getName(), u);

		u = new User("susan");
		u.setPassword("susan");
		allUsers.put(u.getName(), u);
*/
	}

	private Map<String, User> loggedUsers;
	private Map<String, User> allUsers;

	private static UserManager instance = new UserManager();
}
