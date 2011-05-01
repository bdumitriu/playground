package work.chat;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.io.*;

public class AppletChatClient extends JApplet
{
	private String userName;
	
	private JTextArea textArea = new JTextArea(15, 40);
	private JScrollPane scrollPane = new JScrollPane(textArea);
	private JTextField textField = new JTextField();
	private JPanel panel = new JPanel();
	private JLabel label = new JLabel("Your text: ");
	
	private Socket cSocket;
	private BufferedReader br;
	private BufferedWriter bw;
	
	private TextAreaManager listener;
	
	private boolean connectionOK = false;

	public AppletChatClient()
	{
		this("Bogdan");
	}
	
	public AppletChatClient(String userName)
	{
		this.userName = userName;
	}
	
	public void init()
	{
		connectionOK = initializeConnectionWithServer();
		
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
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add("Center", scrollPane);
		getContentPane().add("South", panel);
		
		if (connectionOK)
		{
			textField.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					try
					{
						bw.write(userName + ": " +
							textField.getText());
						bw.newLine();
						bw.flush();
					}
					catch (IOException exception)
					{
						System.out.println(
							exception.getMessage());
						textArea.append(
							"Connetion to server broken.");
					}
					textField.setText("");
				}
			});
			listener = new TextAreaManager();
			listener.start();
		}
		else
		{
			textArea.append("Couldn't connect to the chat server.");
		}
	}
	
	private boolean initializeConnectionWithServer()
	{
		try
		{
			cSocket = new Socket("localhost", 10500);
			
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
			return false;
		}
		
		return true;
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
			//textField.removeActionListener(al);
		}
	}
}
}

