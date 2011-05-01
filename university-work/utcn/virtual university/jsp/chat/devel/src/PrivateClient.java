package ro.utcluj.vu.chat;

import java.rmi.RemoteException;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

/**
 * This class represents the private chat client.
 *
 *	Part of the CHAT subsistem of the Virtual University
 *
 * @author Tudor Marian,
 *	email: <a href="mailto:tudorm@personal.ro">tudorm@personal.ro</a>
 * @version 0.1
 */

public class PrivateClient extends JFrame implements ActionListener
{
	/**
	 * The static attributes - location and size.
	 */
	private static int dimx = 480;
	private static int dimy = 380;
	private int posx;
	private int posy;

	/**
	 * The parent element.
	 */
	private Client parent;

	/**
	 * The parent's server object.
	 */
	private ClassServerInterface server;

	/**
	 * It's self identity.
	 */
	private ClientInterface selfIdentity;

	/**
	 * The os-dependant line separator.
	 */
	private String lineSeparator;

	/**
	 * The graphic "toys".
	 */
	private JButton b1;
	private JTextArea ta;
	private JTextField tf;
	private JScrollPane jsp_ta;
	private JDesktopPane jDesktopPane1;
	private JInternalFrame jInternalFrame1, jInternalFrame3;
	private JMenuBar jMenuBar1;
	private JMenu jMenu1;
	private JMenuItem jMenuItem3, jMenuItem4;
	private JSeparator jSeparator1;

	/**
	 *  the constructor
	 */
	public PrivateClient(Client parent, ClientInterface selfIdentity)
		throws RemoteException
	{
		super(parent.getUserName() + " in private session with " +
			selfIdentity.getUserName());

		this.parent = parent;
		this.selfIdentity = selfIdentity;

		this.server = this.parent.getClassServer();

		this.addWindowListener
			(
				new WindowAdapter()
				{
					public void windowClosing(WindowEvent ev)
					{
						selfRemove();
						ev.getWindow().dispose();
					}
				}
			);

		initComponents();
		lineSeparator = System.getProperty("line.separator");
	}

	/**
	 * The method returns the self identity attribute.
	 */
	public ClientInterface getSelfIdentity()
	{
		return selfIdentity;
	}

	/**
	 * The method sets the self identity attribute.
	 */
	public void setSelfIdentity(ClientInterface selfIdentity)
	{
		this.selfIdentity = selfIdentity;
	}

	/**
	 * The method removes the self object from the parent's private chat
	 * rooms list.
	 */
	private void selfRemove()
	{
		try
		{
			parent.removePrivateChatRoom(this);
		}
		catch (RemoteException e)
		{
		}
	}

	/**
	 * Displays a new incomming private message.
	 */
	public void displayPrivateMessage(ClientInterface from, String message)
		throws RemoteException
	{
		ta.append("<" + from.getUserName() + ">: " + message + lineSeparator);
		ta.setCaretPosition(ta.getText().length());
	}


	/**
	 * The graphic handling and deployment.
	 */
	private void initComponents()
	{
		jDesktopPane1 = new javax.swing.JDesktopPane();
		jInternalFrame1 = new javax.swing.JInternalFrame();
		tf = new javax.swing.JTextField();
		b1 = new javax.swing.JButton();
		jInternalFrame3 = new javax.swing.JInternalFrame();
		ta = new javax.swing.JTextArea();
		jsp_ta = new javax.swing.JScrollPane(ta);
		jMenuBar1 = new javax.swing.JMenuBar();
		jMenu1 = new javax.swing.JMenu();
		jMenuItem3 = new javax.swing.JMenuItem();
		jSeparator1 = new javax.swing.JSeparator();
		jMenuItem4 = new javax.swing.JMenuItem();

		jInternalFrame1.getContentPane().setLayout(new javax.swing.BoxLayout(jInternalFrame1.getContentPane(), javax.swing.BoxLayout.X_AXIS));

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

		jInternalFrame3.setIconifiable(true);
		jInternalFrame3.setMaximizable(true);
		jInternalFrame3.setResizable(true);
		jInternalFrame3.setTitle("Messages");
		jInternalFrame3.setToolTipText("Move and resize all of these to make the chat room appearance match your preferences.");
		jInternalFrame3.setVisible(true);
		ta.setBackground(new Color(255, 255, 255));
		ta.setEditable(false);
		ta.setFont(new java.awt.Font("Lucida Sans", 0, 12));
		//jsp_ta.setAutoscrolls(true);
		jsp_ta.setDoubleBuffered(true);

		jInternalFrame3.getContentPane().add(jsp_ta, java.awt.BorderLayout.CENTER);

		jInternalFrame3.setBounds(10, 80, 420, 240);

		jDesktopPane1.add(jInternalFrame3, javax.swing.JLayeredPane.DEFAULT_LAYER);

		getContentPane().add(jDesktopPane1, java.awt.BorderLayout.CENTER);

		jMenu1.setText("Private room options");
		jMenu1.setMnemonic(KeyEvent.VK_O);
		jMenu1.setToolTipText("Choose some options.");
		jMenuItem3.setText("Save conversation");
		jMenuItem3.setMnemonic(KeyEvent.VK_S);
		jMenuItem3.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem3);
		jMenu1.add(jSeparator1);
		jMenuItem4.setText("Exit");
		jMenuItem4.setMnemonic(KeyEvent.VK_E);
		jMenuItem4.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_E, ActionEvent.CTRL_MASK));
		jMenu1.add(jMenuItem4);
		jMenuBar1.add(jMenu1);
		setJMenuBar(jMenuBar1);

		this.pack();

		b1.addActionListener(this);
		tf.addActionListener(this);
		jMenuItem3.addActionListener(this);
		jMenuItem4.addActionListener(this);

		posx = (int) Math.random() * 640;
		posy = (int) Math.random() * 480;

		this.pack();
		this.setSize(dimx, dimy);
		this.setLocation(posx, posy);
		this.show();
	}

	/**
	 * The listener method.
	 */
	public void actionPerformed(ActionEvent event)
	{
		Object source = event.getSource();

		if (source == b1) //click button
		{
			try
			{
				String message = tf.getText();
				server.sendPrivateMessage(parent, selfIdentity, message);
				ta.append("<" + parent.getUserName() + ">: " + message + lineSeparator);
				ta.setCaretPosition(ta.getText().length());
				tf.setText("");
			}
			catch (RemoteException ex)
			{
				System.out.print("Exception encountered while sending" +
					" private message.");
			}
		}

		if (source == tf) // press return
		{
			try
			{
				String message = tf.getText();
				server.sendPrivateMessage(parent, selfIdentity, message);
				ta.append("<" + parent.getUserName() + ">: " + message + lineSeparator);
				ta.setCaretPosition(ta.getText().length());
				tf.setText("");
			}
			catch (RemoteException ex)
			{
				System.out.print("Exception encountered while sending" +
					" private message.");
			}
		}
		if (source == jMenuItem3)
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
					writer.flush();
					writer.close();
				}
			}
			catch (IOException ex)
			{
				System.out.println("Can't write to file. " + ex);
			}
		}
		if (source == jMenuItem4)
		{
			selfRemove();
			this.dispose();
		}
	}

	/**
	 * The method tests if this object is equal to another.
	 */
	public boolean equals(ClientInterface cli)
		throws RemoteException
	{
		if (selfIdentity.equals(cli))
			return true;

		return false;
	}
}
