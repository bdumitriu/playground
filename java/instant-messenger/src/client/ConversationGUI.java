package client;

import static java.awt.BorderLayout.*;

import javax.swing.*;
import javax.swing.text.Document;
import javax.swing.text.BadLocationException;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class ConversationGUI extends JPanel
{
	public ConversationGUI(String userName, String friendName, IMCore core, JTabbedPane jtp)
	{
		this.core = core;
		this.userName = userName;
		this.friendName = friendName;
		parent = jtp;
		firstPost = true;

		southPanel = new JPanel(new BorderLayout());

		messageTF = new JTextField();
		sendButton = new JButton("Send");
		closeButton = new JButton("Close");

		SendActionListener sal = new SendActionListener();
		messageTF.addActionListener(sal);
		sendButton.addActionListener(sal);
		closeButton.addActionListener(new CloseActionListener());

		convTA = new JTextArea();
		convTA.setEditable(false);
		convScrollPane = new JScrollPane(convTA);

		southPanel.add(closeButton, WEST);
		southPanel.add(messageTF, CENTER);
		southPanel.add(sendButton, EAST);

		setLayout(new BorderLayout());
		add(convScrollPane, CENTER);
		add(southPanel, SOUTH);
	}

	public void postUserMessage(String message)
	{
		postMessage(userName, message);
	}

	public void postFriendMessage(String message)
	{
		postMessage(friendName, message);
	}

	public void postMessage(String user, String message)
	{
		if (firstPost)
		{
			convTA.append("<" + user + "> " + message);
			firstPost = false;
		}
		else
		{
			convTA.append("\n<" + user + "> " + message);
		}
		convTA.setCaretPosition(convTA.getText().length());
	}

	private JPanel southPanel;
	private JTextField messageTF;
	private JButton sendButton;
	private JButton closeButton;
	private JScrollPane convScrollPane;
	private JTextArea convTA;
	private JTabbedPane parent;

	private IMCore core;

	private String friendName;
	private String userName;
	private boolean firstPost;

	class SendActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String message = messageTF.getText();
			if (message.length() > 0)
			{
				core.sendMessage(friendName, message);
				messageTF.setText("");
			}
		}
	}

	class CloseActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			parent.remove(ConversationGUI.this);
		}
	}
}
