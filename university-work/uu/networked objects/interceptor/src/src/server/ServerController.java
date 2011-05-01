package server;

import common.Message;
import common.MessageType;

import javax.swing.*;
import java.net.Socket;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class ServerController extends JFrame
{
	public static void main(String[] args)
	{
		if (args.length != 2)
		{
			System.out.println("Usage: java ServerController <host> <port>\n");
			System.exit(0);
		}

		ServerController sc = new ServerController(args[0], Integer.parseInt(args[1]));

		sc.pack();
		sc.setVisible(true);
	}

	public ServerController(String host, int port)
	{
		super("Server Controller");

		initGUI();

		try
		{
			socket = new Socket(host, port);

			outStr = new ObjectOutputStream(socket.getOutputStream());
			inStr = new ObjectInputStream(socket.getInputStream());
		}
		catch (IOException e)
		{
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

	private void initGUI()
	{
		setLayout(new FlowLayout());
		addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				try
				{
					socket.close();
				}
				catch (IOException e1)
				{
					e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
				finally
				{
					System.exit(0);
				}
			}
		});

		JButton saveButton = new JButton("Save users");
		saveButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				Message inMsg = null;
				try
				{
					outStr.writeObject(new Message(MessageType.SAVE_USER_LIST));
					inMsg = (Message) inStr.readObject();
					if (inMsg.getType() == MessageType.OK)
					{
						JOptionPane.showMessageDialog(ServerController.this,
							"User list saved.", "Info", JOptionPane.INFORMATION_MESSAGE);
					}
				}
				catch (IOException e1)
				{
					e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
				catch (ClassNotFoundException e1)
				{
					e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
			}
		});

		JButton quitButton = new JButton("Quit");
		quitButton.addActionListener(new ActionListener()
		{
			public void actionPerformed(ActionEvent e)
			{
				try
				{
					socket.close();
				}
				catch (IOException e1)
				{
					e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
				finally
				{
					System.exit(0);
				}
			}
		});

		add(saveButton);
		add(quitButton);
	}

	private Socket socket;
	private ObjectOutputStream outStr;
	private ObjectInputStream inStr;
}
