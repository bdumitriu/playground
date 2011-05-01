package work.chat;

import java.io.*;
import java.net.*;
import java.util.*;

public class ChatServer
{
	public static void main(String args[])
	{
		if (args.length == 0)

			new ChatServer();
		else
			try
			{
				new ChatServer(Integer.parseInt(args[0]));
			}
			catch (SocketException e)
			{
				System.out.println(e.getMessage());
			}
		
	}
	
	public ChatServer()
	{
		try
		{
			ServerSocket sSocket = new ServerSocket(10500);
			Socket newSock;
			
			System.out.println("ChatServer: accepting " +
				"connections on port 10500");
			while (true)
			{
				newSock = sSocket.accept();
				Connection con = new Connection(newSock);
				System.out.println("Accetped connection " +
					"from " + newSock.getInetAddress());
				con.start();
			}
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
		}
	}
	
	public ChatServer(int port) throws SocketException
	{
		if ((port <= 0) || (port > 65536))
			throw new SocketException("Invalid port.");
		try
		{
			ServerSocket sSocket = new ServerSocket(port);
			Socket newSock;

			System.out.println("ChatServer: accepting " +
				"connections on port " + port);
			while (true)
			{
				newSock = sSocket.accept();
				Connection con = new Connection(newSock);
				System.out.println("Accetped connection " +
					"from " + newSock.getInetAddress());
				con.start();
			}
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
		}
	}

private static class Connection extends Thread
{
	private static Vector connections = new Vector();
	private Socket sock;
	private BufferedReader br;
	private BufferedWriter bw;
	
	public Connection(Socket sock)
	{
		this.sock = sock;
		try
		{
			br = new BufferedReader(new InputStreamReader(
				sock.getInputStream()));
			bw = new BufferedWriter(new OutputStreamWriter(
				sock.getOutputStream()));
		}
		catch(IOException e)
		{
			System.out.println(e.getMessage());
		}
	}
	
	public void run()
	{
		String charsRead;
		
		try
		{
			connections.addElement(this);

			charsRead = br.readLine();
			while (charsRead != null)
			{
				broadcastMessage(charsRead);
				charsRead = br.readLine();
			}
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
		}
		finally
		{
			connections.removeElement(this);
			try
			{
				sock.close();
			}
			catch (IOException e)
			{
				System.out.println(e.getMessage());
			}
		}
	}
	
	private static void broadcastMessage(String message)
	{
		synchronized (connections)
		{
			Enumeration e = connections.elements();
			while (e.hasMoreElements())
			{
				Connection c = (Connection) e.nextElement();
				try
				{
					synchronized (c.bw)
					{
						c.bw.write(message, 0, message.length());
						c.bw.newLine();
					}
					c.bw.flush();
				}
				catch (IOException ex)
				{
					c.interrupt();
				}
			}
		}
	}
}
}
