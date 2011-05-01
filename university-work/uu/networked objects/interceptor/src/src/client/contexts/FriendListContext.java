package client.contexts;

import client.Friend;

import java.util.ArrayList;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class FriendListContext
{
	public FriendListContext(ArrayList<Friend> friends)
	{
		this.friends = friends;
	}

	public ArrayList<Friend> getFriends()
	{
		return friends;
	}

	private ArrayList<Friend> friends;
}
