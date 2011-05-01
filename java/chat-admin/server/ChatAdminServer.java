package chatAdmin.server;

import java.rmi.*;
import java.rmi.server.*;
import java.io.*;
import java.util.*;
import chatAdmin.*;

/**
 * This class represents a possible implementation of the {@link
 * chatAdmin.ChatAdmin ChatAdmin} interface. Almost all its methods require a
 * userName and a password. The userName required is the one returned by
 * {@link User#getLoginName() getLoginName()} in the {@link User} class. The
 * password is the password of that user. When only the password is required,
 * then the 'admin' password is the one needed.
 *
 * @author Bogdan DUMITRIU
 * @author email: bdumitriu@email.ro
 * @version 1.0
 */
public class ChatAdminServer extends UnicastRemoteObject
	implements ChatAdmin
{
	/*
	 * the clients registered with this server. Clients register with the
	 * purpose of receiving notes of data modifications on the server from
	 * the server. The (key, value) pairs from this TreeMap are like
	 * this:
	 *	(userLoginName, vectorObject)
	 * where the vector holds the references to the clients logged with
	 * the UserLoginName specified by the key. That means that when a new
	 * client registers (specifying a userLoginName and a reference to
	 * itself (let's call it clientRef) as parameters) the server will do
	 * somethig like this:
	 *	Vector v;
	 *	if (connects.containsKey(userLoginName))
	 *		v = (Vector) connects.get(userLoginName);
	 *	else
	 *	{
	 *		v = new Vector();
	 *		connects.put(userLoginName, v);
	 *	}
	 *	v.addElement(clientRef);
	 * thus registering the client in its internal structures.
	 */
	private TreeMap connects;
	
	/*
	 * the chat rooms that exist in this server's structures. The (key,
	 * value) pairs from this TreeMap are of the type:
	 *	(userLoginName, vectorObject)
	 * where the vector holds all the chat rooms that are owned by the
	 * user which has the userLoginName specified by the key.
	 */
	private TreeMap chatRooms;
	
	/*
	 * a structure containing User objects.
	 */
	private Vector users;
	
	/*
	 * a flag used when the administrator wants to lock the server. When
	 * the server is locked everything can be done, except calling the
	 * submitChanges(...) method.
	 */
	private boolean locked = false;

	/**
	 * Creates a new instance of the ChatAdminServer which reads its
	 * data from "chatAdmin/server/data/chatRooms.dat" and
	 * "chatAdmin/server/data/users.dat". That's all it does, no RMI
	 * registration or anything. The user of this class has to do that.
	 */
	public ChatAdminServer() throws RemoteException
	{
		try
		{
			ObjectInputStream inChat = new ObjectInputStream(
				new FileInputStream(
				"chatAdmin/server/data/chatRooms.dat"));
			ObjectInputStream inUser = new ObjectInputStream(
				new FileInputStream(
				"chatAdmin/server/data/users.dat"));
			
			users = (Vector) inUser.readObject();
			chatRooms = (TreeMap) inChat.readObject();
		}
		catch (InvalidClassException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (OptionalDataException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (ClassNotFoundException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (FileNotFoundException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (StreamCorruptedException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * Returns a vector containing all the {@link ChatRoom}s owned by the
	 * specified user. If that user is 'admin' than all {@link ChatRoom}s
	 * are returned.
	 * <br><br>
	 */
	public Vector getChatRooms(String userName, String password)
		throws RemoteException, UserNotFoundException,
		InvalidPasswordException
	{
		validate(userName, password);

		if (userName.equals("admin"))
		{
			synchronized (chatRooms)
			{
				Vector retVector = new Vector();
				Vector tmp;
				Collection col = chatRooms.values();
				Iterator itCol = col.iterator();
				while (itCol.hasNext())
				{
					tmp = (Vector) itCol.next();
					retVector.addAll(tmp);
				}
				return retVector;
			}
		}
		else
		{
			synchronized (chatRooms)
			{
				return (Vector) chatRooms.get(userName);
			}
		}
	}

	/**
	 * Returns a vector of {@link User}s containing all the users
	 * registered in the system. The password needed is the 'admin'
	 * password.
	 * <br><br>
	 */
	public Vector getUsers(String password) throws RemoteException,
		InvalidPasswordException
	{
		try
		{
			validate("admin", password);
			synchronized (users)
			{
				return users;
			}
		}
		catch (UserNotFoundException e)
		{
			System.out.println("ChatAdminServer critical:" +
				" superuser 'admin' does not exist. " +
				"Server behaviour can not be guaranteed!");
			return null;
		}
	}

	/**
	 * Returns the {@link User} object corresponding to the specified
	 * userName.
	 */
	public User getUser(String userName, String password)
		throws RemoteException, UserNotFoundException,
		InvalidPasswordException
	{
		User user = validate(userName, password);
		return user;
	}

	/**
	 * Locks access to the server. This means that until {@link
	 * #unlock(String) unlock()} will be called no one except 'admin'
	 * will be able to successfully call {@link
	 * submitChanges(String, String, Vector, Vector) submitChanges()}.
	 */
	public void lockAccess(String password) throws RemoteException,
		InvalidPasswordException
	{
		try
		{
			validate("admin", password);
			locked = true;
			// clients.accessLocked();
		}
		catch (UserNotFoundException e)
		{
			System.out.println("ChatAdminServer critical:" +
				" superuser 'admin' does not exist. " +
				"Server behaviour can not be guaranteed!");
		}
	}

	/**
	 * Used to unlock the server and so make it fuction normally again.
	 * See {@link lock(String password)} for further details.
	 */
	public void unlockAccess(String password) throws RemoteException,
		InvalidPasswordException
	{
		try
		{
			validate("admin", password);
			locked = false;
		}
		catch (UserNotFoundException e)
		{
			System.out.println("ChatAdminServer critical:" +
				" superuser 'admin' does not exist. " +
				"Server behaviour can not be guaranteed!");
		}
	}
	
	/**
	 * Method used to obtain current state of server. It returns true if
	 * server is locked and false otherwise.\
	 */
	public boolean isLocked() throws RemoteException
	{
		return locked;
	}

	/**
	 * The method by which changes can be made visible on the server.
	 * There are two cases:
	 * <ul>
	 * <li> if 'admin' calls this method...:<br>
	 * ... then this is what happens: newUsers is checked and everything
	 * that isn't an instance of {@link User} is thrown away. After this
	 * is done, the stripped down vector replaces the one on the server.
	 * The same happens with the newChatRooms. The only 'extra' for the
	 * latter is that another check is done to see if all chat rooms have
	 * valid owners. The ones that don't are thrown away. Valid owner
	 * means that the owner is part of the newUsers vector.<br>
	 * <li> if a normal user calls this method...<br>
	 * ... then the newUsers vector is searched for the first occurence
	 * of an {@link User} object with the same loginName as the userName
	 * parameter. This is taken out (if found) and then the rest of the
	 * vector is thrown away. If such an object was found it replaces the
	 * old entry with the same loginName on the server. After that, the
	 * newChatRooms vector is searched for chat rooms belonging to the
	 * specified user and they are added to the server (up to the limit
	 * of {@link User#getMaxChatRooms()} of the {@link User}).
	 */
	public void submitChanges(String userName, String password,
		Vector newChatRooms, Vector newUsers) throws RemoteException,
		UserNotFoundException, InvalidPasswordException, LockException
	{
		if ((!locked) || (userName.equals("admin")))
		{
			validate(userName, password);
			if (userName.equals("admin")) // admin superuser
			{
				adminChanges(newUsers, newChatRooms);
			}
			else //normal user
			{
				userChanges(userName, newUsers, newChatRooms);
			}
		}
		else
			throw new LockException();
	}

	/**
	 * Writes the server data to the same files specified in the class
	 * constructor ({@link #ChatAdmin()}.
	 */
	public void stopServer()
	{
		try
		{
			ObjectOutputStream outChat = new ObjectOutputStream(
				new FileOutputStream(
				"chatAdmin/server/data/chatRooms.dat"));
			ObjectOutputStream outUser = new ObjectOutputStream(
				new FileOutputStream(
				"chatAdmin/server/data/users.dat"));
			
			synchronized (chatRooms)
			{
				outChat.writeObject(chatRooms);
			}
			synchronized (users)
			{
				outUser.writeObject(users);
			}
		}
		catch (FileNotFoundException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		catch (Throwable e)
		{
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}

	/*
	 * Checks if the specified userName and password are valid on the
	 * server, throwing exceptions otherwise. If successful, it returns
	 * the User object that corresponds to the userName.
	 */
	private User validate(String userName, String password)
		throws UserNotFoundException, InvalidPasswordException
	{
		synchronized (users)
		{
			int idx = users.indexOf(new User(userName, password));
			if (idx == -1)
			{
				System.out.println("Attempt to connect " +
					"using '" + userName + "' as " +
					"userName failed.");
				throw new UserNotFoundException("User " +
					userName + " has not been found " +
					" in the server database.");
			}
			else
			{
				User user = (User) users.elementAt(idx);
				if (!(user.getPassword().equals(password)))
				{
					System.out.println("Wrong password " +
						"for user " + userName + ".");
					throw new InvalidPasswordException(
						"The password supplied for " +
						"user " + userName +
						" is not valid.");
				}
				System.out.println("User " + userName +
					" validated.");
				return user;
			}
		}
	}

	private void adminChanges(Vector newUsers, Vector newChatRooms)
	{
		int size = newUsers.size();
		Object obj;
		Vector idxs = new Vector();
		for (int i = 0; i < size; i++)
		{
			obj = newUsers.elementAt(i);
			if (obj instanceof User)
			{
				idxs.addElement(new Integer(
					addUser((User) obj)));
			}
		}
		size = users.size();
		for (int i = size-1; i >= 0; i--)
		{
			if (!(idxs.contains(new Integer(i))))
			{
				//User user = (User) users.elementAt(i);
				users.removeElementAt(i);
				// clients.userDeleted(user)
				// (only for user user.getLoginName() unless
				// it is admin, in which case we don't do a
				// callback)
			}
		}

		synchronized (chatRooms)
		{
			User user;
			chatRooms.clear();

			synchronized (users)
			{
				size = users.size();
				for (int i = 0; i < size; i++)
				{
					user = (User) users.elementAt(i);
					chatRooms.put(user.getLoginName(),
						new Vector());
				}
			}

			size = newChatRooms.size();
			for (int i = 0; i < size; i++)
			{
				obj = newChatRooms.elementAt(i);
				if (obj instanceof ChatRoom)
				{
					ChatRoom cr = (ChatRoom) obj;
					String oln = cr.getOwnerLoginName();
					if (users.contains(new User(oln, "")))
					{
						Vector v = (Vector)
							chatRooms.get(oln);
						v.addElement(cr);
					}
				}
			}
		}
		
		synchronized (users)
		{
			size = users.size();
			for (int i = 0; i < size; i++)
				checkIntegrity((User) users.elementAt(i));
		}
		//clients.chatRoomsModified() (all but admin)
	}

	private void userChanges(String userName, Vector newUsers,
		Vector newChatRooms)
	{
		int idx = newUsers.indexOf(new User(userName, ""));
		if (idx != -1)
		{
			User user = (User) newUsers.elementAt(idx);
			String newPass = user.getPassword();

			synchronized (users)
			{
				idx = users.indexOf(new User(userName, ""));
				if (idx != -1)
				{
					user = (User) users.elementAt(idx);
					user.setPassword(newPass);
				}
			}
		}

		synchronized (chatRooms)
		{
			if (chatRooms.containsKey(userName))
			{
				Vector v = new Vector();
				chatRooms.put(userName, v);
				int size = newChatRooms.size();
				for (int i = 0;	i < size; i++)
				{
					Object obj = newChatRooms.elementAt(i);
					if (obj instanceof ChatRoom)
					{
						ChatRoom cr = (ChatRoom) obj;
						String oln =
							cr.getOwnerLoginName();
						if (oln.equals(userName))
						{
							v.addElement(cr);
						}
					}
				}
				idx = users.indexOf(new User(userName, ""));
				if (idx != -1)
				{
					User user =
						(User) users.elementAt(idx);
					checkIntegrity(user);
				}
			}
		}
	}

	/*
	 * Adds the user 'user' to the users Vector and returns its position in the
	 * vector.
	 */
	private int addUser(User user)
	{
		int idx;
		synchronized (users)
		{
			idx = users.indexOf(user);
			if (idx != -1) // user already exists => it is changed
			{
				User u = (User) users.elementAt(idx);
				u.setContactInfo(user.getContactInfo());
				u.setMaximumNumberOfChatRooms(
					user.getMaximumNumberOfChatRooms());
				u.setName(user.getName());
				u.setPassword(user.getPassword());
				// clients.userModified(u)
				// (only for user user.getLoginName() unless
				// it is admin, in which case we don't do
				// a callback.
			}
			else // user doesn't exist => it is simply added
			{
				users.addElement(user);
				// since the element is added at the end of the
				// vector we can do what follows and keep things
				// correct. This is faster than
				// 	idx = users.indexOf(user);
				// which would be the alternative.
				idx = users.size()-1;
			}
		}
		return idx;
	}

	/*
	 * Checks to see if the user user doesn't have more chat rooms than he
	 * is allowed to.
	 */
	private void checkIntegrity(User user)
	{
		synchronized (chatRooms)
		{
			byte max = user.getMaximumNumberOfChatRooms();
			if (max == 0)
				return;
			Vector v = (Vector) chatRooms.get(user.getLoginName());
			if (max >= v.size())
				return;
			ChatRoom vectorArray[] = new ChatRoom[v.size()];
			v.toArray(vectorArray);
			Arrays.sort(vectorArray, new ChatRoomComparator());
			v.removeAllElements();
			for (int i = 0; i < max; i++)
				v.addElement(vectorArray[i]);
		}
	}
}
