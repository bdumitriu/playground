package client;

import static common.MessageType.*;
import static client.contexts.Outcome.*;
import static client.contexts.MessageOutcome.*;

import common.Message;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.StringTokenizer;
import java.util.ArrayList;
import java.net.Socket;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

import client.dispatchers.LoginDispatcher;
import client.dispatchers.FriendDispatcher;
import client.dispatchers.MessageDispatcher;
import client.contexts.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 19, 2005
 */
public class IMCore
{
	public IMCore(String host, int port)
	{
		serverHost = host;
		serverPort = port;
		loggedIn = false;
		userName = "";

		loginDispatcher = LoginDispatcher.getInstance();
		friendDispatcher = FriendDispatcher.getInstance();
		messageDispatcher = MessageDispatcher.getInstance();
	}

	public void logIn(String userName, String password)
	{
		if (!loggedIn)
		{
			this.userName = userName;
			LoginContext ctx = new LoginContext(Outcome.NONE, userName);
			loginDispatcher.preLogin(ctx);
			outThread = new OutThread(this);
			outThread.start();
			outThread.putMessage(new Message(LOG_IN, userName, password));
		}
	}

	public void logOut()
	{
		if (loggedIn)
		{
			LoginContext ctx = new LoginContext(Outcome.NONE, userName);
			loginDispatcher.preLogout(ctx);
			outThread.putMessage(new Message(LOG_OUT));
			outThread.putMessage(new Message(FINISH_THREAD));
			loggedIn = false;
		}
	}

	public void addFriend(String name)
	{
		if (loggedIn)
		{
			outThread.putMessage(new Message(ADD_FRIEND, name));
		}
	}

	public void getFriendsList()
	{
		if (loggedIn)
		{
			outThread.putMessage(new Message(GET_FRIENDS));
		}
	}

	public void sendMessage(String to, String message)
	{
		if (loggedIn)
		{
			MessageContext ctx = new MessageContext(MessageOutcome.NONE, userName, to, message);
			messageDispatcher.preMessageSend(ctx);
			outThread.putMessage(new Message(MESSAGE, ctx.getToUser(), ctx.getMessage()));
		}
	}

	synchronized public void process(Message inMsg, Message outMsg)
	{
		switch (inMsg.getType())
		{
			case LOG_OUT:
			{
				processLogOut(inMsg);
				break;
			}
			case PORT_INFO:
			{
				processPortInfo(inMsg);
				break;
			}
			case WRONG_USER_PASS:
			{
				LoginContext ctx = new LoginContext(FAILED, inMsg.getValue2());
				loginDispatcher.postLogin(ctx);
				break;
			}
			case NOT_A_USER:
			{
				if (outMsg.getType() == ADD_FRIEND)
				{
					FriendContext ctx = new FriendContext(FAILED, userName, inMsg.getValue1());
					friendDispatcher.friendAdded(ctx);
				}
				else if (outMsg.getType() == MESSAGE)
				{
					MessageContext ctx = new MessageContext(MessageOutcome.NOT_A_USER, userName,
						outMsg.getValue1(), outMsg.getValue2());
					messageDispatcher.messageSent(ctx);
				}
				break;
			}
			case NOT_A_FRIEND:
			{
				if (outMsg.getType() == MESSAGE)
				{
					MessageContext ctx = new MessageContext(MessageOutcome.NOT_A_FRIEND, userName,
						outMsg.getValue1(), outMsg.getValue2());
					messageDispatcher.messageSent(ctx);
				}
				break;
			}
			case NOT_ONLINE:
			{
				if (outMsg.getType() == MESSAGE)
				{
					MessageContext ctx = new MessageContext(MessageOutcome.NOT_ONLINE, userName,
						outMsg.getValue1(), outMsg.getValue2());
					messageDispatcher.messageSent(ctx);
				}
				break;
			}
			case ADD_FRIEND:
			{
				processAddFriend(inMsg);
				break;
			}
			case FRIEND_LOGIN:
			{
				FriendContext ctx = new FriendContext(Outcome.NONE, userName, inMsg.getValue1());
				friendDispatcher.wentOnline(ctx);
				break;
			}
			case FRIEND_LOGOUT:
			{
				FriendContext ctx = new FriendContext(Outcome.NONE, userName, inMsg.getValue1());
				friendDispatcher.wentOffline(ctx);
				break;
			}
			case MESSAGE:
			{
				MessageContext ctx = new MessageContext(MessageOutcome.NONE, inMsg.getValue1(), userName,
					inMsg.getValue2());
				messageDispatcher.messageReceived(ctx);
				break;
			}
			case OK:
			{
				processOk(inMsg, outMsg);
				break;
			}
		}
	}

	private void processLogOut(Message inMsg)
	{
		inThread.triggerExit();
		outThread.putMessage(new Message(FINISH_THREAD));

		loggedIn = false;

		LoginContext ctx = new LoginContext(Outcome.NONE, userName);
		loginDispatcher.postLogout(ctx);
	}

	private void processPortInfo(Message inMsg)
	{
		inThread = new InThread(this, Integer.parseInt(inMsg.getValue1()));
		inThread.start();

		loggedIn = true;

		LoginContext ctx = new LoginContext(SUCCESSFUL, inMsg.getValue2());
		loginDispatcher.postLogin(ctx);
	}

	private void processAddFriend(Message inMsg)
	{
		FriendContext ctx = new FriendContext(SUCCESSFUL, userName, inMsg.getValue1());
		friendDispatcher.friendAdded(ctx);

		ctx = new FriendContext(Outcome.NONE, userName, inMsg.getValue1());
		friendDispatcher.wentOnline(ctx);
	}

	private void processOk(Message inMsg, Message outMsg)
	{
		switch (outMsg.getType())
		{
			case ADD_FRIEND:
			{
				FriendContext ctx = new FriendContext(SUCCESSFUL, userName, inMsg.getValue1());
				friendDispatcher.friendAdded(ctx);

				if (inMsg.getValue2().equals("online"))
				{
					ctx = new FriendContext(Outcome.NONE, userName, inMsg.getValue1());
					friendDispatcher.wentOnline(ctx);
				}

				break;
			}
			case GET_FRIENDS:
			{
				StringTokenizer stNames = new StringTokenizer(inMsg.getValue1(), ";");
				StringTokenizer stStatuses = new StringTokenizer(inMsg.getValue2(), ";");

				ArrayList<Friend> friends = new ArrayList<Friend>();

				while (stNames.hasMoreElements())
				{
					String name = stNames.nextToken();
					String statusMessage = stStatuses.nextToken();
					if (statusMessage.equals("offline"))
					{
						friends.add(new Friend(name));
					}
					else if (statusMessage.equals("online"))
					{
						friends.add(new Friend(name, FriendStatus.ONLINE));
					}
					else
					{
						friends.add(new Friend(name, statusMessage, FriendStatus.ONLINE));
					}
				}

				FriendListContext ctx = new FriendListContext(friends);
				friendDispatcher.receivedFriendsList(ctx);

				break;
			}
			case MESSAGE:
			{
				MessageContext ctx = new MessageContext(MessageOutcome.OK, userName, outMsg.getValue1(),
					outMsg.getValue2());
				messageDispatcher.messageSent(ctx);
				break;
			}
		}
	}

	public String getServerHost()
	{
		return serverHost;
	}

	public int getServerPort()
	{
		return serverPort;
	}

	private LoginDispatcher loginDispatcher;
	private FriendDispatcher friendDispatcher;
	private MessageDispatcher messageDispatcher;

	private String userName;
	private boolean loggedIn;

	private InThread inThread;
	private OutThread outThread;

	private String serverHost;
	private int serverPort;
}

class InThread extends Thread
{
	public InThread(IMCore core, int port)
	{
		serverHost = core.getServerHost();
		serverPort = port;
		this.core = core;
		exit = false;
	}

	public void triggerExit()
	{
		exit = true;
	}

	public void run()
	{
		try
		{
			Socket socket = new Socket(serverHost, serverPort);

			ObjectInputStream inStr = new ObjectInputStream(socket.getInputStream());
			//ObjectOutputStream outStr = new ObjectOutputStream(socket.getOutputStream());

			while (!exit)
			{
				Message inMsg = (Message) inStr.readObject();
				core.process(inMsg, null);
			}

			socket.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private boolean exit;

	private String serverHost;
	private int serverPort;
	IMCore core;
}

class OutThread extends Thread
{
	public OutThread(IMCore core)
	{
		this.core = core;
		queue = new LinkedBlockingQueue<Message>();
	}

	public void putMessage(Message msg)
	{
		try
		{
			queue.put(msg);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	public void run()
	{
		try
		{
			Socket socket = new Socket(core.getServerHost(), core.getServerPort());

			ObjectOutputStream outStr = new ObjectOutputStream(socket.getOutputStream());
			ObjectInputStream inStr = new ObjectInputStream(socket.getInputStream());

			boolean exit = false;
			do
			{
				Message outMsg = queue.take();
				if (outMsg.getType() == FINISH_THREAD)
				{
					exit = true;
				}
				else
				{
					outStr.writeObject(outMsg);
					if (outMsg.getType() == LOG_IN)
					{
						Message inMsg = (Message) inStr.readObject();
						if (inMsg.getType() == WRONG_USER_PASS)
						{
							exit = true;
						}
						core.process(inMsg, outMsg);
					}
					else
					{
						Message inMsg = (Message) inStr.readObject();
						core.process(inMsg, outMsg);
					}
				}
			}
			while (!exit);

			socket.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (ClassNotFoundException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private BlockingQueue<Message> queue;
	private IMCore core;
}