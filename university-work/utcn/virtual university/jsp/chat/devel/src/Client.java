package ro.utcluj.vu.chat;

import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;

import java.util.*;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.io.*;

/**
 * Client.java
 *
 * The main client, used to connect to class server, and eventually chat..
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */

public class Client extends JFrame implements ClientInterface, ActionListener
{
	/**
	 * The static attributes giving size and location.
	 */
	private static int posx = 350;
	private static int posy = 350;
	private static int dimx = 650;
	private static int dimy = 385;

	/**
	 * Message in case unable to connect to server.
	 */
	private String errMsg = new String(
		"Unable to connect to server. Shoud the chat server be stopped, \n" +
		"wich is higly inprobable, but possible, please excuse us for the \n" +
		"trouble. Please try agai later. However you should do the following \n" +
		"operations: Please check to see if you are \n" +
		"behind a firewall. If you are, check with your firewall \n" +
		"administrator to enable your tcp stream on standard RMI port (1099).\n" +
		"If you are behind a PROXY server (such as squid) the RMI protocol \n" +
		"should be able to perform conenctions. If it does not, check with your \n" +
		"proxy server administrator and advise him to activate the java provided \n" +
		"script found in the standard jdk package that allows tunneling for the \n" +
		"RMI protocol as well.\n" + "Thank you for your understanding. \n" +
		"The developer, Tudor Marian.\n"
	);

	/**
	 * The tips data.
	 */
	String tip1 = new String(
		"The work zones within the actual frame are\n" +
		"inner frames themselves, wich means that they\n" +
		"can be moved within the parent frame as desired."
	);

	String tip2 = new String(
		"You can choose to disconnect at any given time\n" +
		"and you can allways connect back. Beware of more\n" +
		"than one chat client used with the same user ID \n" +
		" only one client can be used at a time."
	);

	String tip3 = new String(
		"Should you want to save to a text file the conversation\n" +
		"displayed within the text area, the save to file action\n" +
		"must be invoked. It can be found within the main menu\n" +
		"under the \"Save conversation\" tag, or you can use the\n" +
		"<Ctrl+S> hotkey."
	);

	private String[] tips = new String[]{tip1, tip2, tip3};

	/**
	 * The attribute that gives the status of the connection.
	 */
	private boolean connected = false;

	/**
	 * The attribute that indicates that another client with same
	 * user ID is logged on.
	 */
	private boolean inactive = false;

	private String userID;
	private String classID;
	private String className;
	private String userName;

	/**
	 * The list of users.
	 */
	private Vector userList;

	/**
	 * The list of user names.
	 */
	private Vector nameUserList;

	/**
	 * The list of private chat rooms.
	 */
	private Map privateChatRooms;

	/**
	 * The self object used in remote calls.
	 */
	private static ClientInterface self;

	/**
	 * The remote factory server.
	 */
	private static RemoteServerInterface factory;

	/**
	 * The class server.
	 */
	private ClassServerInterface server;

	/**
	 * The chat sever's address.
	 */
	private String host;

	/**
	 * The chat server's port.
	 */
	private int port;

	/**
	 * The new line separator.
	 */
	private String lineSeparator;

	/**
	 * The graphic "toys".
	 */
	private JButton b1;
	private JButton b2;
	private JTextArea ta;
	private JTextField tf;
	private JList lst;
	private JScrollPane jsp_ta, jsp_lst;
	private JDesktopPane jDesktopPane1;
	private JInternalFrame jInternalFrame1, jInternalFrame2,
	jInternalFrame3;
	private JLabel lbl;
	private JMenuBar jMenuBar1;
	private JMenu jMenu1, jMenu2;
	private JMenuItem jMenuItem1, jMenuItem2, jMenuItem3, jMenuItem4, jMenuItem5, jMenuItem6;
	private JSeparator jSeparator1, jSeparator2;

	/**
	 * The constructor.
	 */
	public Client(String userID, String classID, String userName,
		String className)
	{
		super(userName + " on class \" " + className + " \"");

		userList = new Vector();
		nameUserList = new Vector();
		privateChatRooms = new HashMap();

		this.userID = userID;
		this.classID = classID;
		this.userName = userName;
		this.className = className;

		this.addWindowListener
			(
				new WindowAdapter()
				{
					public void windowClosing(WindowEvent ev)
					{
						if (inactive)
						{
							// got disconnected from server
							Client.this.dispose();
						}
						else if (connected)
						{
							disconnect();
						}
						else
						{
							//System.exit(0);
							Client.this.dispose();
						}
					}
				}
			);

		initComponents();

		lineSeparator = System.getProperty("line.separator");
	}

	/**
	 * The method checks to see if 2 objects are equal. The criterion
	 * is the user ID attribute.
	 */
	public boolean equals(ClientInterface object)
		throws RemoteException
	{
		if (this.getUserID().equals(object.getUserID()))
			return true;
		else
			return false;
	}

	/**
	 * This method displays a pulbic message.
	 */
	public void receivePublicMessage(ClientInterface from, String message)
		throws RemoteException
	{
		ta.append("<" + from.getUserName() + ">: " + message + lineSeparator);
		ta.setCaretPosition(ta.getText().length());
	}

	/**
	 * The method sets the inactivity flag.
	 */
	public void setInactive()
		throws RemoteException
	{
		this.inactive = true;
		disconnect();
	}

	/**
	 * This method displays a private message on the appropriate private chat
	 * room if one exists, if it does not it will create one and post the
	 * message.
	 */
	public void receivePrivateMessage(ClientInterface from, String message)
		throws RemoteException
	{
		PrivateClient pc = getPrivateClient(from);
		pc.show();
		pc.displayPrivateMessage(from, message);
	}


	/**
	 * This method posts the service message from the server.
	 */
	public void receiveServiceMessage(String message)
		throws RemoteException
	{
		ta.append("<SERVER>: " + message + lineSeparator);
		ta.setCaretPosition(ta.getText().length());
	}

	/**
	 * The method returns the user ID attribute.
	 */
	public String getUserID() throws RemoteException
	{
		return this.userID;
	}

	/**
	 * The method returns the user name attribute.
	 */
	public String getUserName() throws RemoteException
	{
		return this.userName;
	}

	/**
	 * The method announces the client that a certain user
	 * got disconnected.
	 */
	public void receiveUserDisconnected(ClientInterface object)
		throws RemoteException
	{
		PrivateClient pc = (PrivateClient) privateChatRooms.get(
			object.getUserID()
		);

		if (pc != null)
		{
			pc.dispose();
			privateChatRooms.remove(object.getUserID());
		}
	}

	/**
	 * The metohd sets the host name and the port attributes.
	 */
	public void setAdressAttributes(String host, int port)
	{
		this.host = host;
		this.port = port;
	}

	/**
	 * The method returns the class server attribute.
	 */
	public ClassServerInterface getClassServer()
	{
		return this.server;
	}

	/**
	 * The method connects the client to the given chat server.
	 */
	public void rmiConnect()
	{
		try
		{
			self = (ClientInterface) this;

			Registry reg = LocateRegistry.getRegistry(host, port);
			factory = (RemoteServerInterface) reg.lookup("factory");

			this.resolveServer();
		}
		catch (RemoteException ex)
		{
			connected = false;
			jInternalFrame3.setTitle("Messages (Not connected)");
			ta.append(errMsg);
			ta.setCaretPosition(ta.getText().length());
			jMenuItem2.setEnabled(false);
			jMenuItem1.setEnabled(true);
		}
		catch (NotBoundException ex)
		{
			connected = false;
			jInternalFrame3.setTitle("Messages (Not connected)");
			ta.append(errMsg);
			jMenuItem2.setEnabled(false);
			jMenuItem1.setEnabled(true);
		}
	}

	/**
	 * The method disconnects the client from the chat server.
	 */
	public void disconnect()
	{
		if (inactive)
		{
			server = null;
			jMenuItem2.setEnabled(false);
			jMenuItem1.setEnabled(true);
			jInternalFrame3.setTitle("Messages (Not connected)");
		}
		if (connected)
		{
			try
			{
				server.requestLogout(self);
				UnicastRemoteObject.unexportObject(this, true);
				closePrivateChatRooms();

				server = null;

				nameUserList.clear();
				lst.setListData(nameUserList);

				jMenuItem2.setEnabled(false);
				jMenuItem1.setEnabled(true);
				jInternalFrame3.setTitle("Messages (Not connected)");
			}
			catch (RemoteException ex)
			{
			}
			connected = false;
		}
	}

	/**
	 * This method obtains the class server from the factory server.
	 */
	public void resolveServer()
	{
		try
		{
			server = factory.getClassServer(this.classID);
			if (!inactive)
				UnicastRemoteObject.exportObject(this);

			if (server.requestLogin(self))
			{
				jInternalFrame3.setTitle("Messages (Connected)");
				connected = true;
				inactive = false;
				jMenuItem2.setEnabled(true);
				jMenuItem1.setEnabled(false);
				ta.append(server.getLoginMessage());
			}
			else
			{
				connected = false;
				ta.append(server.getLoginMessage());
				setInactive();
			}

		}
		catch (RemoteException ex)
		{
			System.out.println("Exception encountered during class server"
				+ " aquire process");
			ex.printStackTrace();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}


	/**
	 * The mouse listener method, used to select the client to private chat to
	 */
	private class MyMouseListener extends MouseAdapter
	{
		public void mouseClicked(MouseEvent event)
		{
			if (event.getClickCount() == 2)
			{
				try
				{
					//get the location of the double click
					int index = lst.locationToIndex(event.getPoint());

					//get the name of the client
					String privateClientName = (String) nameUserList.get(index);
					ClientInterface clientInterface = (ClientInterface)
						userList.get(index);

					if (!clientInterface.getUserName().equals(privateClientName))
					{
						System.out.println("noncoresponding user name");
					}

					if (!self.equals(clientInterface))
						getPrivateClient(clientInterface).show();
				}
				catch (Exception ex)
				{
					System.out.println("Exception encountered opening private"
						+ " chat room.");
					ex.printStackTrace();
				}
			}
		}
	}

	/**
	 * The method returns a private client object if one exists into the private
	 * chat rooms list, or creates a new one if it does not.
	 */
	private PrivateClient getPrivateClient(ClientInterface cli)
		throws RemoteException
	{
		PrivateClient pc = null;

		if (privateChatRooms.containsKey(cli.getUserID()))
		{
			pc = (PrivateClient) privateChatRooms.get(cli.getUserID());
		}
		else
		{
			pc = new PrivateClient(this, cli);
			privateChatRooms.put(cli.getUserID(), pc);
		}

		return pc;
	}

	/**
	 * The method closes all private chat rooms.
	 */
	private void closePrivateChatRooms()
	{
		try
		{
			PrivateClient c;
			Map.Entry entry;

			Set rooms = privateChatRooms.entrySet();
			Iterator iterator = rooms.iterator();

			while (iterator.hasNext())
			{
				entry = (Map.Entry) iterator.next();
				c = (PrivateClient) entry.getValue();
				c.dispose();
			}

			privateChatRooms.clear();
		}
		catch (Exception ex)
		{
			System.out.println("exception encountered" + ex);
		}
	}

	/**
	 * The method updates the user list.
	 */
	public void updateUserList(Vector list)
		throws RemoteException
	{
		userList.clear();
		nameUserList.clear();

		ClientInterface user;

		for (int i = 0; i < list.size(); i++)
		{
			user = (ClientInterface) list.get(i);
			userList.add(user);
			nameUserList.add(user.getUserName());
		}
		lst.setListData(nameUserList);
	}

	/**
	 * The method removes a private client.
	 */
	public void removePrivateChatRoom(PrivateClient pc)
		throws RemoteException
	{
		privateChatRooms.remove(pc.getSelfIdentity().getUserID());
	}

	/**
	 *  The action listener resolving method.
	 */
	public void actionPerformed(ActionEvent event)
	{

		Object source = event.getSource();

		if (source == b1) //click "send message" button
		{
			try
			{
				if (connected)
					server.sendPublicMessage(self, tf.getText());
				tf.setText("");
			}
			catch (RemoteException ex)
			{
				System.out.print("exception encountered while sending message..");
			}
		}
		else if (source == tf) // press return
		{
			try
			{
				if (connected)
					server.sendPublicMessage(self, tf.getText());
				tf.setText("");
			}
			catch (RemoteException ex)
			{
				System.out.print("exception encountered while sending message..");
			}
		}
		else if (source == jMenuItem1) // connect
		{
			rmiConnect();
		}
		else if (source == jMenuItem2) // disconnect
		{
			disconnect();
		}
		else if (source == jMenuItem3) // save conversation
		{

			JFileChooser fileChooser = new JFileChooser();

			fileChooser.setDialogTitle("Choose or create a new file to store the conversation");
			fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			fileChooser.setDoubleBuffered(true);

			fileChooser.showOpenDialog(this);

			File file = fileChooser.getSelectedFile();

			try
			{
				if (file != null)
				{

					Writer writer = new BufferedWriter(new FileWriter(file));

					writer.write(ta.getText());
					writer.close();
				}
			}
			catch (IOException ex)
			{
				System.out.println("Can't write to file " + ex);
			}
		}
		else if (source == jMenuItem4) // exit & disconnect
		{
			if (connected)
				disconnect();
			this.dispose();
		}
		else if (source == jMenuItem5) // tips
		{
			JOptionPane.showMessageDialog(this,
				generateTip(), "Tip of the day",
				JOptionPane.INFORMATION_MESSAGE
			);
		}
		else if (source == jMenuItem6) // about
		{
			JOptionPane.showMessageDialog(this,
				"Developed as the chat subsistem belonging to \n" +
				"the comunications sistem of the Virtual University.\n" +
				"\nCreated by Tudor Marian <tudorm@personal.ro>\n" +
				"Computer Science Department\n" +
				"Technical University Of Cluj-Napoca",
				"About the chat client",
				JOptionPane.INFORMATION_MESSAGE);
		}
	}

	/**
	 * The method generates tips string messages.
	 */
	private String generateTip()
	{
		long millis = System.currentTimeMillis();
		int idx = (int) (millis % (tips.length));

		return tips[idx];
	}

	/**
	 *  The graphic component handle & deployment
	 */
	private void initComponents()
	{
		jDesktopPane1 = new javax.swing.JDesktopPane();
		jInternalFrame1 = new javax.swing.JInternalFrame();
		tf = new javax.swing.JTextField();
		b1 = new javax.swing.JButton();
		jInternalFrame2 = new javax.swing.JInternalFrame();
		jsp_lst = new javax.swing.JScrollPane();
		lst = new javax.swing.JList();
		jInternalFrame3 = new javax.swing.JInternalFrame();
		ta = new javax.swing.JTextArea();
		jsp_ta = new javax.swing.JScrollPane(ta);
		jMenuBar1 = new javax.swing.JMenuBar();
		jMenu1 = new javax.swing.JMenu();
		jMenu2 = new javax.swing.JMenu();
		jMenuItem1 = new javax.swing.JMenuItem();
		jMenuItem2 = new javax.swing.JMenuItem();
		jMenuItem3 = new javax.swing.JMenuItem();
		jMenuItem4 = new javax.swing.JMenuItem();
		jMenuItem5 = new javax.swing.JMenuItem();
		jMenuItem6 = new javax.swing.JMenuItem();
		jSeparator1 = new javax.swing.JSeparator();
		jSeparator2 = new javax.swing.JSeparator();

		jInternalFrame1.getContentPane().setLayout(
			new javax.swing.BoxLayout(
				jInternalFrame1.getContentPane(), javax.swing.BoxLayout.X_AXIS
			)
		);

		jInternalFrame1.setIconifiable(true);
		jInternalFrame1.setMaximizable(true);
		jInternalFrame1.setResizable(true);
		jInternalFrame1.setTitle("Message editor");
		jInternalFrame1.setToolTipText("Move and resize all of these to make the chat room appearance match your preferences.");
		jInternalFrame1.setVisible(true);
		tf.setFont(new java.awt.Font("Lucida Sans", 0, 12));
		jInternalFrame1.getContentPane().add(tf);

		b1.setText("Send Message");
		jInternalFrame1.getContentPane().add(b1);

		jInternalFrame1.setBounds(10, 10, 440, 60);
		jDesktopPane1.add(jInternalFrame1, javax.swing.JLayeredPane.DEFAULT_LAYER);

		jInternalFrame2.setIconifiable(true);
		jInternalFrame2.setMaximizable(true);
		jInternalFrame2.setResizable(true);
		jInternalFrame2.setTitle("User list");
		jInternalFrame2.setToolTipText("Move and resize all of these to make the chat room appearance match your preferences.");
		jInternalFrame2.setVisible(true);
		lst.setFont(new java.awt.Font("Lucida Sans", 0, 12));
		jsp_lst.setViewportView(lst);

		jInternalFrame2.getContentPane().add(jsp_lst, java.awt.BorderLayout.CENTER);

		jInternalFrame2.setBounds(490, 80, 140, 240);
		jDesktopPane1.add(jInternalFrame2, javax.swing.JLayeredPane.DEFAULT_LAYER);

		jInternalFrame3.setIconifiable(true);
		jInternalFrame3.setMaximizable(true);
		jInternalFrame3.setResizable(true);
		jInternalFrame3.setTitle("Messages (Not connected)");
		jInternalFrame3.setToolTipText("Move and resize all of these to make the chat room appearance match your preferences.");
		jInternalFrame3.setVisible(true);
		ta.setBackground(new java.awt.Color(255, 255, 255));
		ta.setEditable(false);
		ta.setFont(new java.awt.Font("Lucida Sans", 0, 12));
		//jsp_ta.setAutoscrolls(true);
		jsp_ta.setDoubleBuffered(true);

		jInternalFrame3.getContentPane().add(jsp_ta, java.awt.BorderLayout.CENTER);

		jInternalFrame3.setBounds(10, 80, 450, 240);
		jDesktopPane1.add(jInternalFrame3, javax.swing.JLayeredPane.DEFAULT_LAYER);

		getContentPane().add(jDesktopPane1, java.awt.BorderLayout.CENTER);

		jMenu1.setText("Chat options");
		jMenu1.setMnemonic(KeyEvent.VK_O);
		jMenu1.setToolTipText("Choose some options.");
		jMenuItem1.setText("Connect");
		jMenuItem1.setMnemonic(KeyEvent.VK_C);
		jMenuItem1.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem1);
		jMenuItem2.setText("Disconnect");
		jMenuItem2.setMnemonic(KeyEvent.VK_D);
		jMenuItem2.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_D, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem2);
		jMenuItem3.setText("Save conversation");
		jMenuItem3.setMnemonic(KeyEvent.VK_S);
		jMenuItem3.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem3);
		jMenu1.add(jSeparator1);
		jMenuItem4.setText("Quit");
		jMenuItem4.setMnemonic(KeyEvent.VK_Q);
		jMenuItem4.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem4);

		jMenu2.setText("Help");
		jMenu2.setMnemonic(KeyEvent.VK_H);
		jMenuItem5.setText("Tips");
		jMenuItem5.setMnemonic(KeyEvent.VK_T);
		jMenuItem5.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_T, ActionEvent.CTRL_MASK));
		jMenu2.add(jMenuItem5);
		jMenu2.add(jSeparator2);
		jMenuItem6.setText("About");
		jMenuItem6.setMnemonic(KeyEvent.VK_A);
		jMenuItem6.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_A, ActionEvent.CTRL_MASK));
		jMenu2.add(jMenuItem6);

		jMenuBar1.add(jMenu1);
		jMenuBar1.add(jMenu2);
		setJMenuBar(jMenuBar1);

		b1.addActionListener(this);
		jMenuItem1.addActionListener(this);
		jMenuItem2.addActionListener(this);
		jMenuItem3.addActionListener(this);
		jMenuItem4.addActionListener(this);
		jMenuItem5.addActionListener(this);
		jMenuItem6.addActionListener(this);
		tf.addActionListener(this);
		lst.addMouseListener(new MyMouseListener());

		this.pack();
		this.setSize(dimx, dimy);
		this.setLocation(posx, posy);
		this.show();
	}

	/**
	 * The method sets the self attribute.
	 */
	public void setSelf()
	{
		self = (ClientInterface) this;
	}

	/**
	 * This method is used to create the "stand alone" client
	 */
	public static void main(String[] args)
	{
		if (args.length != 5)
		{
			System.out.println("Usage: java Client <userID> <classID>" +
				" <userName> <className> <server>");
			System.exit(1);
		}

		//System.setSecurityManager(new RMISecurityManager());
		try
		{
			Client frame = new Client(args[0], args[1], args[2], args[3]);
			frame.setSelf();

			frame.setAdressAttributes(args[4], 1099);

			frame.rmiConnect();
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
		}
	}
}
