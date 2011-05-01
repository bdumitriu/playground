package work.chat;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;

/**
 * This class creates an instance of a chat client that connects to
 * a chat server and shows the user a graphical interface which s/he
 * can use to chat with other people connected to the same chat server.
 *
 * To use it, just instantiate it like this:
 * <pre>
 * 	new SwingChatClient("My Name", "chat.server.org", 10500);
 * </pre>
 *
 * @author Bogdan DUMITRIU
 * @version 1.0
 */

public class SwingChatClient
{
	private String userName;
	
	/*
		I need all these as class fields because I acces them
		form various places, not just from the buildGUI() method.
	*/
	private JFrame frame = new JFrame("Bogdan's Chat Room");
	private JTextArea textArea = new JTextArea(15, 40);
	private JScrollPane scrollPane = new JScrollPane(textArea);
	private JTextField textField = new JTextField();
	private JPanel panel = new JPanel();
	private JLabel label = new JLabel("Your text here: ");
	private TextFieldActionListener al;

	private Socket cSocket;
	private BufferedReader br;
	private BufferedWriter bw;
	
	private TextAreaManager listener;

	private boolean connectionOK = false;

	/**
	 * Creates the chat client using <code>192.168.0.2</code> as
	 * host and <code>10500</code> as port.
	 *
	 * @param userName the name which the client will append at the
	 * 	beggining of the user's messages.
	 * @see #SwingChatClient(String userName, String host, int port)
	 */
	public SwingChatClient(String userName)
	{
		this(userName, "192.168.0.2", 10500);
	}
	
	/**
	 * Creates the chat client using <code>10500</code> as port.
	 *
	 * @param userName the name which the client will append
	 * 	at the beggining of the user's messages.
	 * @param host the machine on which the chat server is running.
	 * @see #SwingChatClient(String userName, String host, int port)
	 */
	public SwingChatClient(String userName, String host)
	{
		this(userName, host, 10500);
	}
	
	/**
	 * Creates the chat client using the specified parameters. This means
	 * it creates the GUI, connects to the server and, if the connection
	 * is successful, allows the user to chat.
	 *
	 * @param userName the name which the client will append
	 * 	at the beggining of the user's messages.
	 * @param host the machine on which the chat server is running.
	 * @param port the port on which the chat server is running.
	 */
	public SwingChatClient(String userName, String host, int port)
	{
		/*
			Initialize the user name. This is what the other
			chat partners will see as the nickname of the one
			using this chat client.
		*/
		this.userName = userName;
		
		/*
			Create the graphical user interface.
		*/
		buildGUI();
		
		/*
			Try to connect to the chat server.
		*/
		connectionOK = initializeConnectionWithServer(host, port);
		
		/*
			Take different actions depending on the success of
			the connection.
		*/
		if (connectionOK)
		{
			textField.addActionListener(al =
				new TextFieldActionListener());
			listener = new TextAreaManager();
			listener.start();
		}
		else
		{
			textArea.append(
				"Couldn't connect to the chat server.\n");
		}
	}

	/*
		This private method takes care of building the GUI for the
		chat client.
	*/
	private void buildGUI()
	{
		frame.addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				System.exit(0);
			}
		});
		frame.setSize(400, 400);
		scrollPane.setBorder(new LineBorder(Color.lightGray, 6));
		textArea.setEditable(false);
		textArea.setLineWrap(true);
		textArea.setWrapStyleWord(true);
		textArea.setBackground(Color.darkGray);
		textArea.setForeground(Color.white);
		label.setBackground(Color.lightGray);
		panel.setLayout(new BorderLayout());
		panel.setBorder(new LineBorder(Color.lightGray, 2));
		panel.add("Center", textField);
		panel.add("West", label);
		frame.getContentPane().setLayout(new BorderLayout());
		frame.getContentPane().add("Center", scrollPane);
		frame.getContentPane().add("South", panel);
		frame.pack();
		frame.setVisible(true);
	}
	
	/*
		This private method (hopefully) opens a connection to the
		chat server on the specified host/port.
	*/
	private boolean initializeConnectionWithServer(String host, int port)
	{
		try
		{
			cSocket = new Socket(host, port);
			
			br = new BufferedReader(new InputStreamReader(
				cSocket.getInputStream()));
			bw = new BufferedWriter(new OutputStreamWriter(
				cSocket.getOutputStream()));
		}
		catch (UnknownHostException e)
		{
			System.out.println(e.getMessage());
			return false;
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			try
			{
				cSocket.close();
			}
			catch (Exception exception)
			{}
			return false;
		}
		
		return true;
	}

/*
	This class handles the action that has to be taken when the user
	presses "Enter" after writing text in the text field.
*/
private class TextFieldActionListener implements ActionListener
{
	public void actionPerformed(ActionEvent event)
	{
		try
		{
			bw.write(userName + ": " + textField.getText());
			bw.newLine();
			bw.flush();
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			textArea.append("Connection to server lost.\n");
			try
			{
				cSocket.close();
			}
			catch (Exception exception)
			{}
			finally
			{
				textField.removeActionListener(al);
			}
		}
		finally
		{
			textField.setText("");
		}
	}
}

private class TextAreaManager extends Thread
{
	public void run()
	{
		String charsRead;
		
		try
		{
			charsRead = br.readLine();
			while (charsRead != null)
			{
				textArea.append(charsRead + "\n");
				charsRead = br.readLine();
			}
		}
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			textArea.append("Connection to server lost.\n");
		}
		finally
		{
			try
			{
				cSocket.close();
			}
			catch (Exception e)
			{}
			textField.removeActionListener(al);
		}
	}
}

}
