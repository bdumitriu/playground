package server;

import static common.MessageType.*;
import common.Message;

import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Set;
import java.net.ServerSocket;
import java.io.IOException;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class Processor
{
	public static Processor getInstance()
	{
		return instance;
	}

	public Message process(Message inMsg, InThread inThread)
	{
		switch (inMsg.getType())
		{
			case LOG_IN:
			{
				return processLogin(inMsg, inThread);
			}
			case LOG_OUT:
			{
				return processLogout(inThread);
			}
			case ADD_FRIEND:
			{
				return processAddFriend(inMsg, inThread);
			}
			case GET_FRIENDS:
			{
				return processGetFriends(inThread);
			}
			case MESSAGE:
			{
				return processMessage(inMsg, inThread);
			}
			case SAVE_USER_LIST:
			{
				return processSaveUserList();
			}
			default:
			{
				return new Message(NOT_UNDERSTOOD);
			}
		}
	}

	private Message processLogin(Message inMsg, InThread inThread)
	{
		Message outMsg = new Message();
		outMsg.setValue2(inMsg.getValue1());
		User u = um.getUser(inMsg.getValue1());

		if (u == null)
		{
			outMsg.setType(WRONG_USER_PASS);
		}
		else
		{
			if (u.getPassword().equals(inMsg.getValue2()))
			{
				try
				{
					ServerSocket sSocket = new ServerSocket(0);

					OutThread outThread = new OutThread(sSocket);

					outThread.setUserName(u.getName());
					inThread.setUserName(u.getName());

					outThread.start();

					outMsg.setType(PORT_INFO);
					outMsg.setValue1(Integer.toString(sSocket.getLocalPort()));

					addUserThreads(new UserThreads(u.getName(), inThread, outThread));

					um.setOnline(u.getName(), true);

					// notify friends that (s)he went online
					ArrayList<User> friends = um.getOnlineFriendsOf(u.getName());
					Message msg = new Message(FRIEND_LOGIN, u.getName());
					for (User user : friends)
					{
						userThreads.get(user.getName()).getOutThread().putMessage(msg);
					}
				}
				catch (IOException e)
				{
					e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
			}
			else
			{
				outMsg.setType(WRONG_USER_PASS);
			}
		}

		return outMsg;
	}

	private Message processLogout(InThread inThread)
	{
		removeUserThreads(inThread.getUserName());
		um.setOnline(inThread.getUserName(), false);

		// notify friends that (s)he went offline
		ArrayList<User> friends = um.getOnlineFriendsOf(inThread.getUserName());
		Message outMsg = new Message(FRIEND_LOGOUT, inThread.getUserName());
		for (User user : friends)
		{
			userThreads.get(user.getName()).getOutThread().putMessage(outMsg);
		}

		return new Message(OK);
	}

	private Message processAddFriend(Message inMsg, InThread inThread)
	{
		String userName = inThread.getUserName();
		String friendName = inMsg.getValue1();
		if (um.addFriend(userName, friendName))
		{
			// add this user as a friend of the newly added friend
			um.addFriend(friendName, userName);

			String s;
			if (um.isOnline(friendName))
			{
				s = "online";
				userThreads.get(friendName).getOutThread().putMessage(new Message(ADD_FRIEND, userName));
			}
			else
			{
				s =  "offline";
			}

			return new Message(OK, inMsg.getValue1(), s);
		}
		else
		{
			return new Message(NOT_A_USER, inMsg.getValue1());
		}
	}

	private Message processGetFriends(InThread inThread)
	{
		ArrayList<User> onlineFriends = um.getOnlineFriendsOf(inThread.getUserName());
		if (onlineFriends == null)
		{
			return new Message(OK);
		}
		else
		{
			Set<String> allFriends = um.getUser(inThread.getUserName()).getFriends();

			StringBuilder sbNames = new StringBuilder();
			StringBuilder sbStatuses = new StringBuilder();
			for (String userName : allFriends)
			{
				sbNames.append(userName);
				sbNames.append(";");
				int index = onlineFriends.indexOf(new User(userName));
				if (index == -1)
				{
					sbStatuses.append("offline");
				}
				else
				{
					String friendStatus = onlineFriends.remove(index).getStatus();

					if (friendStatus.length() > 0)
					{
						sbStatuses.append(friendStatus);
					}
					else
					{
						sbStatuses.append("online");
					}
				}

				sbStatuses.append(";");
			}

			String names = "";
			if (sbNames.length() > 0)
			{
				names = sbNames.substring(0, sbNames.length() - 1);
			}
			String status = "";
			if (sbStatuses.length() > 0)
			{
				status = sbStatuses.substring(0, sbStatuses.length() - 1);
			}
			return new Message(OK, names, status);
		}
	}

	private Message processMessage(Message inMsg, InThread inThread)
	{
		String from = inThread.getUserName();
		String to = inMsg.getValue1();
		String message = inMsg.getValue2();
		User toUser = um.getUser(to);

		if (toUser == null)
		{
			return new Message(NOT_A_USER);
		}
		else if (!(toUser.getFriends().contains(from)))
		{
			return new Message(NOT_A_FRIEND);
		}
		else if (!um.isOnline(to))
		{
			return new Message(NOT_ONLINE);
		}
		else
		{
			userThreads.get(to).getOutThread().putMessage(new Message(MESSAGE, from, message));
			return new Message(OK);
		}
	}

	private Message processSaveUserList()
	{
		um.saveUsers();

		return new Message(OK);
	}

	synchronized private void addUserThreads(UserThreads userTh)
	{
		UserThreads oldUserTh = userThreads.get(userTh.getUserName());
		if (oldUserTh != null)
		{
			oldUserTh.getInThread().triggerExit();
			oldUserTh.getOutThread().putMessage(new Message(LOG_OUT));
			oldUserTh.getOutThread().putMessage(new Message(FINISH_THREAD));
			try
			{
				oldUserTh.getOutThread().join();
			}
			catch (InterruptedException e)
			{
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}
		}

		userThreads.put(userTh.getUserName(), userTh);
	}

	synchronized private void removeUserThreads(String userName)
	{
		UserThreads oldUserTh = userThreads.get(userName);
		if (oldUserTh != null)
		{
			oldUserTh.getInThread().triggerExit();
			oldUserTh.getOutThread().putMessage(new Message(LOG_OUT));
			oldUserTh.getOutThread().putMessage(new Message(FINISH_THREAD));
			userThreads.remove(userName);
		}
	}

	private Processor()
	{
		um = UserManager.getInstance();
		userThreads = new HashMap<String, UserThreads>();
	}

	private UserManager um;
	private Map<String, UserThreads> userThreads;

	private static Processor instance = new Processor();
}
