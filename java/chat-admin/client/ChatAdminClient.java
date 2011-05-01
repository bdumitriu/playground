package chatAdmin.client;

import java.rmi.*;
import java.util.Vector;
import java.util.TreeMap;
import chatAdmin.ChatAdmin;
import chatAdmin.server.*;

public class ChatAdminClient
{
	/*public static void main(String args[])
	{
		try
		{
			ChatAdmin client = (ChatAdmin) Naming.lookup("//192" +
				".168.0.2:2500/chatAdminServ");
			
			Vector users = new Vector();
			users.add(new User("admin", "aaaaaa"));
			users.add(new User("bdumitriu", "bbbbbb"));
			users.add(new User("tudorm", "cccccc"));
			users.add(new User("danamrc", "dddddd"));
			Vector chatRooms = new Vector();
			chatRooms.add(new ChatRoom("cr1", "admin"));
			chatRooms.add(new ChatRoom("cr2", "admin"));
			chatRooms.add(new ChatRoom("cr3", "tudorm"));
			chatRooms.add(new ChatRoom("cr4", "admin"));
			chatRooms.add(new ChatRoom("cr5", "tudorm"));
			chatRooms.add(new ChatRoom("cr6", "danamrc"));
			client.submitChanges("admin", "aaaaaa", chatRooms,
				users);
			
			System.out.println(client.getUsers("aaaaaa"));
			System.out.println(client.getChatRooms("admin", "aaaaaa"));
		}
		catch (Exception e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}*/
	
	public static void main(String args[])
	{
		try
		{
			ChatAdmin client = (ChatAdmin) Naming.lookup("//192" +
				".168.0.1:2500/chatAdminServ");

			char com;
			int n;
			byte read[] = new byte[50];
			String user = "admin";
			String pass = "aaaaaa";
			String x, y, z;
			byte a, b;
			User u;
			ChatRoom c;
			Vector users = new Vector();
			Vector crs = new Vector();
			users.add(new User("admin", "aaaaaa"));
			
			do
			{
			commands();
			System.in.skip(System.in.available());
			com = (char) System.in.read();
			switch (com)
			{
				case '1':
				{
				System.out.print("New user: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				user = new String(read, 0, n-2);
				System.out.println("New user is " + user +
					".");
				System.in.read();
				break;
				}
				case '2':
				{
				System.out.print("New password (user is " +
					user + "): ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				pass = new String(read, 0, n-2);
				System.out.println("New password is " + pass +
					".");
				System.in.read();
				break;
				}
				case '3':
				{
				System.out.print("Login name: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				x = new String(read, 0, n-2);
				System.out.print("Password: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				y = new String(read, 0, n-2);
				System.out.print("Max nr. of chat rooms: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				z = new String(read, 0, n-2);
				a = Byte.parseByte(z);
				u = new User(x, y);
				u.setMaximumNumberOfChatRooms(a);
				users.add(u);
				System.out.println("User " + x + " added " +
					" with " + y + " as password and " +
					"with " + a + " chat rooms.");
				System.in.read();
				break;
				}
				case '4':
				{
				System.out.print("Login name: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				x = new String(read, 0, n-2);
				n = users.indexOf(new User(x, ""));
				if (n != -1)
				{
					users.removeElementAt(n);
					System.out.println("User " + x +
						" deleted.");
				}
				else
				{
					System.out.println("User " + x +
						" not found.");
				}
				System.in.read();
				break;
				}
				case '5':
				{
				crs.removeAllElements();
				System.out.println("Chat room vector is now" +
					" empty.");
				System.in.read();
				break;
				}
				case '6':
				{
				System.out.print("Chat room name: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				x = new String(read, 0, n-2);
				System.out.print("Chat room owner: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				y = new String(read, 0, n-2);
				System.out.print("Chat room priority: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				z = new String(read, 0, n-2);
				a = Byte.parseByte(z);
				c = new ChatRoom(x, y);
				c.setPriority(a);
				crs.add(c);
				System.out.println("Chat room " + x +
					" added with " + y + " as owner and "
					+ "with priority " + a + ".");
				System.in.read();
				break;
				}
				case '7':
				{
				System.out.print("Chat room name: ");
				System.in.skip(System.in.available());
				n = System.in.read(read, 0, 20);
				x = new String(read, 0, n-2);
				n = crs.indexOf(new ChatRoom(x, ""));
				if (n != -1)
				{
					crs.removeElementAt(n);
					System.out.println("Chat room " + x +
						" deleted.");
				}
				else
				{
					System.out.println("Chat room " + x +
						" not found.");
				}
				System.in.read();
				break;
				}
				case '8':
				{
				System.out.println("The users on the server "
					+ "are: ");
				System.out.println(client.getUsers(pass));
				System.in.read();
				break;
				}
				case '9':
				{
				System.out.println("The chat rooms on the "
					+ "server are: ");
				System.out.println(client.getChatRooms(
					user, pass));
				System.in.read();
				break;
				}
				case 's':
				{
				client.submitChanges(user, pass, crs, users);
				System.out.println("Changes submitted.");
				System.in.read();
				break;
				}
				case 'u':
				{
				System.out.println(users);
				System.in.read();
				break;
				}
				case 'c':
				{
				System.out.println(crs);
				System.in.read();
				break;
				}
				case '0':
				{
				break;
				}
				default:
				{
				System.out.println("Command not recognized.");
				System.in.read();
				break;
				}
			}
			}
			while (com != '0');
			
		}
		catch (UserNotFoundException e)
		{
			System.out.println("Server couldn't find the user.");
		}
		catch (InvalidPasswordException e)
		{
			System.out.println("Server couldn't identify user " +
				"with the password you supplied");
		}
		catch (Exception e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}
	
	private static void commands()
	{
		System.out.println();
		System.out.println("1. change user name");
		System.out.println("2. change password");
		System.out.println("3. add user");
		System.out.println("4. delete user");
		System.out.println("5. clear chat room vector");
		System.out.println("6. add chat room");
		System.out.println("7. delete chat room");
		System.out.println("8. list all users");
		System.out.println("9. list all chat rooms");
		System.out.println("s. submit changes");
		System.out.println("u. print users");
		System.out.println("c. print chat rooms");
		System.out.println("0. exit");
		System.out.println();
		System.out.print("Your command: ");
	}
}
