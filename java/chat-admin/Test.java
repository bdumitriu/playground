package chatAdmin;

import java.io.*;
import java.util.*;
import chatAdmin.server.*;

public class Test
{
	public static void main(String a[])
	{
		try
		{
			Vector users = new Vector();
			TreeMap chats = new TreeMap();
			users.addElement(new User("admin", "aaaaaa"));
			chats.put("admin", new Vector());
			ObjectOutputStream outChat = new ObjectOutputStream(
				new FileOutputStream(
				"chatAdmin/server/data/chatRooms.dat"));
			ObjectOutputStream outUser = new ObjectOutputStream(
				new FileOutputStream(
				"chatAdmin/server/data/users.dat"));
			outChat.writeObject(chats);
			outUser.writeObject(users);
		}
		catch (Exception e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
			System.exit(0);
		}
	}
}
