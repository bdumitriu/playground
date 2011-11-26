package server;

import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.io.*;

import common.Message;
import static common.MessageType.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class IMServer extends Thread
{
	public IMServer(int port)
	{
		this.port = port;
	}

	public void run()
	{
		try
		{
			ServerSocket sSocket = new ServerSocket(port);
			while (true)
			{
				Socket cSocket = sSocket.accept();
				InThread inThread = new InThread(cSocket);
				inThread.start();
			}
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}

	}

	private int port;
}

class UserThread extends Thread
{
	public UserThread()
	{
		this.processor = Processor.getInstance();
	}

	public String getUserName()
	{
		return userName;
	}

	public void setUserName(String userName)
	{
		this.userName = userName;
	}

	protected String userName;
	protected Processor processor;
}

class InThread extends UserThread
{
	public InThread(Socket socket)
	{
		this.socket = socket;
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
			ObjectInputStream inStr = new ObjectInputStream(socket.getInputStream());
			ObjectOutputStream outStr = new ObjectOutputStream(socket.getOutputStream());

			while (!exit)
			{
				Message inMsg = (Message) inStr.readObject();
				Message outMsg = processor.process(inMsg, this);
				outStr.writeObject(outMsg);
				if (outMsg.getType() == WRONG_USER_PASS)
				{
					exit = true;
				}
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

	private Socket socket;
	private boolean exit;
}

class OutThread extends UserThread
{
	public OutThread(ServerSocket socket)
	{
		this.socket = socket;
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
			Socket sock = socket.accept();
			socket.close();

			//ObjectInputStream inStr = new ObjectInputStream(sock.getInputStream());
			ObjectOutputStream outStr = new ObjectOutputStream(sock.getOutputStream());

			boolean exit = false;
			do
			{
				Message msg = queue.take();
				if (msg.getType() == FINISH_THREAD)
				{
					exit = true;
				}
				else
				{
					outStr.writeObject(msg);
				}
			}
			while (!exit);

			sock.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private BlockingQueue<Message> queue;
	private ServerSocket socket;
}