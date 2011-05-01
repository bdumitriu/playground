package client;

import client.dispatchers.LoginDispatcher;
import client.dispatchers.FriendDispatcher;
import client.dispatchers.MessageDispatcher;
import client.interceptors.*;
import client.contexts.LoginContext;
import client.contexts.FriendContext;
import client.contexts.FriendListContext;
import client.contexts.MessageContext;

import static java.awt.BorderLayout.*;
import static client.contexts.Outcome.*;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;
import java.awt.*;
import java.awt.event.*;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 18, 2005
 */
public class IMGUI extends JFrame implements LoginInterceptor, FriendInterceptor, MessageInterceptor
{
	public IMGUI(IMCore core)
	{
		super("Proof-of-concept Instant Messenger");

		this.core = core;

		userName = "bdumitriu";
		friends = new TreeSet<Friend>();

		initGUI();

		LoginDispatcher.getInstance().addLoginInterceptor(this);
		FriendDispatcher.getInstance().addFriendInterceptor(this);
		MessageDispatcher.getInstance().addMessageInterceptor(this, 1000);
	}

	public void initGUI()
	{
		JMenuBar jmb = new JMenuBar();
		setJMenuBar(jmb);

		JMenu intMenu = new JMenu("Interceptors");
		jmb.add(intMenu);

		loggerInt = new Logger();
		showLoggerMI = new JCheckBoxMenuItem("Show logger");
		enableLoggerMI = new JCheckBoxMenuItem("Enable logger");

		translatorInt = new Translator(true);
		noTranslationRB = new JRadioButtonMenuItem("No translation");
		noTranslationRB.setSelected(true);
		deTranslationRB = new JRadioButtonMenuItem("Dutch -> English");
		edTranslationRB = new JRadioButtonMenuItem("English -> Dutch");

		ButtonGroup bg = new ButtonGroup();
		bg.add(noTranslationRB);
		bg.add(deTranslationRB);
		bg.add(edTranslationRB);

		MenuActionListener mai = new MenuActionListener();
		showLoggerMI.addActionListener(mai);
		enableLoggerMI.addActionListener(mai);
		noTranslationRB.addActionListener(mai);
		deTranslationRB.addActionListener(mai);
		edTranslationRB.addActionListener(mai);

		intMenu.add(showLoggerMI);
		intMenu.add(enableLoggerMI);
		intMenu.add(new JSeparator());
		intMenu.add(noTranslationRB);
		intMenu.add(deTranslationRB);
		intMenu.add(edTranslationRB);
		intMenu.add(new JSeparator());

		mainPanel = new JPanel(new BorderLayout());
		leftPanel = new JPanel(new BorderLayout());

		friendsList = new JList();
		friendsList.setDoubleBuffered(true);
		friendsList.addMouseListener(new FriendsMouseListener());
		friendsList.setCellRenderer(new FriendsListCellRenderer());
		listScrollPane = new JScrollPane(friendsList);

		logInPanel = new JPanel();
		logInPanel.setLayout(new GridLayout(3, 2));

		userTF = new JTextField(8);
		userLabel = new JLabel();
		userLabel.setText("User Name:");
		userLabel.setLabelFor(userTF);

		passTF = new JPasswordField(8);
		passLabel = new JLabel();
		passLabel.setText("Password:");
		passLabel.setLabelFor(passTF);

		logInButton = new JButton();
		logInButton.setText("Log In");
		logInButton.addActionListener(new LogInActionListener());

		logOutButton = new JButton();
		logOutButton.setText("Log Out");
		logOutButton.addActionListener(new LogOutActionListener());

		conversationTP = new JTabbedPane(JTabbedPane.BOTTOM);
		conversationTP.addChangeListener(new ChangeListener()
		{
			public void stateChanged(ChangeEvent e)
			{
				int index = conversationTP.getSelectedIndex();
				if (index != -1)
				{
					conversationTP.setForegroundAt(index, null);
				}
			}
		});

		JPanel tmpPanel;

		tmpPanel = new JPanel();
		tmpPanel.add(userLabel);
		logInPanel.add(tmpPanel);
		tmpPanel = new JPanel();
		tmpPanel.add(userTF);
		logInPanel.add(tmpPanel);
		tmpPanel = new JPanel();
		tmpPanel.add(passLabel);
		logInPanel.add(tmpPanel);
		tmpPanel = new JPanel();
		tmpPanel.add(passTF);
		logInPanel.add(tmpPanel);
		tmpPanel = new JPanel();
		tmpPanel.add(logInButton);
		logInPanel.add(tmpPanel);
		tmpPanel = new JPanel();
		tmpPanel.add(logOutButton);
		logInPanel.add(tmpPanel);

		leftPanel.add(listScrollPane, CENTER);
		leftPanel.add(logInPanel, SOUTH);

		mainPanel.add(leftPanel, WEST);
		mainPanel.add(conversationTP, CENTER);

		getContentPane().add(mainPanel);
		addWindowListener(new WindowAdapter()
		{
			public void windowClosing(WindowEvent e)
			{
				core.logOut();
			}
		});

		friendsPopup = new JPopupMenu("Actions");
		friendsPopup.add("Add friend").addActionListener(new AddFriendAction());
		friendsPopup.add("Remove friend").addActionListener(new RemoveFriendAction());
	}

	public ConversationGUI ensureTab(String friend, boolean alsoSelect)
	{
		int index = conversationTP.indexOfTab(friend);
		ConversationGUI cg;
		if (index == -1)
		{
			cg = new ConversationGUI(userName, friend, core, conversationTP);
			conversationTP.addTab(friend, cg);
		}
		else
		{
			cg = (ConversationGUI) conversationTP.getComponentAt(index);
		}

		if (alsoSelect)
		{
			index = conversationTP.indexOfTab(friend);
			conversationTP.setSelectedIndex(index);
		}

		return cg;
	}

	public void preLogin(LoginContext ctx)
	{}

	public void postLogin(LoginContext ctx)
	{
		if (ctx.getOutcome() == FAILED)
		{
			JOptionPane.showMessageDialog(this, "Login failed.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			JOptionPane.showMessageDialog(this, "Login successful.", "Info", JOptionPane.INFORMATION_MESSAGE);
			userName = ctx.getUserName();
			core.getFriendsList();
		}
	}

	public void preLogout(LoginContext ctx)
	{}

	public void postLogout(LoginContext ctx)
	{
		friends.clear();
		friendsList.setListData(friends.toArray());
		JOptionPane.showMessageDialog(this, "Logged out.", "Info", JOptionPane.INFORMATION_MESSAGE);
	}

	public void friendAdded(FriendContext ctx)
	{
		if (ctx.getOutcome() == FAILED)
		{
			JOptionPane.showMessageDialog(this, "\"" + ctx.getFriendName() + "\" is not a valid user.", "Error", JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			friends.add(new Friend(ctx.getFriendName()));
			friendsList.setListData(friends.toArray());
			JOptionPane.showMessageDialog(this, "User \"" + ctx.getFriendName() + "\" has become your friend.", "Info", JOptionPane.INFORMATION_MESSAGE);
		}
	}

	public void friendRemoved(FriendContext ctx)
	{
		//To change body of implemented methods use File | Settings | File Templates.
	}

	public void wentOnline(FriendContext ctx)
	{
		friends.remove(new Friend(ctx.getFriendName()));
		friends.add(new Friend(ctx.getFriendName(), FriendStatus.ONLINE));
		friendsList.setListData(friends.toArray());
	}

	public void wentOffline(FriendContext ctx)
	{
		friends.remove(new Friend(ctx.getFriendName()));
		friends.add(new Friend(ctx.getFriendName(), FriendStatus.OFFLINE));
		friendsList.setListData(friends.toArray());
	}

	public void receivedFriendsList(FriendListContext ctx)
	{
		friends.clear();
		friends.addAll(ctx.getFriends());
		friendsList.setListData(friends.toArray());
	}

	public void preMessageSend(MessageContext ctx)
	{}

	public void messageSent(MessageContext ctx)
	{
		switch (ctx.getOutcome())
		{
			case NOT_A_USER:
			{
				// shouldn't be the case
				break;
			}
			case NOT_A_FRIEND:
			{
				// shouldn't be the case
				break;
			}
			case NOT_ONLINE:
			{
				JOptionPane.showMessageDialog(this, ctx.getToUser() + " is not online.", "Error",
					JOptionPane.ERROR_MESSAGE);
				break;
			}
			case OK:
			{
				ensureTab(ctx.getToUser(), false).postUserMessage(ctx.getMessage());
				break;
			}
		}
	}

	public void messageReceived(MessageContext ctx)
	{
		ensureTab(ctx.getFromUser(), false).postFriendMessage(ctx.getMessage());

		int index = conversationTP.indexOfTab(ctx.getFromUser());
		if (index != conversationTP.getSelectedIndex())
		{
			conversationTP.setForegroundAt(index, Color.RED);
		}
	}

	private JPanel mainPanel, leftPanel, logInPanel;
	private JScrollPane listScrollPane;
	private JList friendsList;
	private JTextField userTF;
	private JPasswordField passTF;
	private JLabel userLabel, passLabel;
	private JButton logInButton, logOutButton;
	private JTabbedPane conversationTP;
	private JPopupMenu friendsPopup;
	private JCheckBoxMenuItem showLoggerMI, enableLoggerMI;
	private JRadioButtonMenuItem noTranslationRB, deTranslationRB, edTranslationRB;
	private Logger loggerInt;
	private Translator translatorInt;

	private String userName;
	private SortedSet<Friend> friends;

	private IMCore core;

	private int[] loggerIntIds = new int[3];
	private int translatorIntId;
	private int selectedTranslation = 0;

	class LogInActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			core.logIn(userTF.getText(), new String(passTF.getPassword()));
			userName = userTF.getText();
		}
	}

	class LogOutActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			core.logOut();
		}
	}

	class FriendsMouseListener extends MouseAdapter
	{
		public void mouseClicked(MouseEvent e)
		{
			if (SwingUtilities.isRightMouseButton(e))
			{
				friendsPopup.show(e.getComponent(), e.getX(), e.getY());
			}
			else
			{
				if (e.getClickCount() == 2)
				{
					Friend friend = (Friend) friendsList.getSelectedValue();
					ensureTab(friend.getName(), true);
				}
			}
		}
	}

	class AddFriendAction implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			String friend = JOptionPane.showInputDialog(mainPanel, "Friend's name:");
			if (friend != null) // Ok was pressed
			{
				core.addFriend(friend);
			}
		}
	}

	class RemoveFriendAction implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{

		}
	}

	class MenuActionListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			if (e.getSource() == showLoggerMI)
			{
				if (showLoggerMI.isSelected())
				{
					loggerInt.setVisible(true);
				}
				else
				{
					loggerInt.setVisible(false);
				}
			}
			else if (e.getSource() == enableLoggerMI)
			{
				if (enableLoggerMI.isSelected())
				{
					loggerIntIds[0] = LoginDispatcher.getInstance().addLoginInterceptor(loggerInt);
					loggerIntIds[1] = FriendDispatcher.getInstance().addFriendInterceptor(loggerInt);
					MessageDispatcher.getInstance().addMessageInterceptor(loggerInt, 999);
					loggerIntIds[2] = 999;
				}
				else
				{
					LoginDispatcher.getInstance().removeLoginInterceptor(loggerIntIds[0]);
					FriendDispatcher.getInstance().removeFriendInterceptor(loggerIntIds[1]);
					MessageDispatcher.getInstance().removeMessageInterceptor(loggerIntIds[2]);
				}
			}
			else if (e.getSource() == noTranslationRB)
			{
				MessageDispatcher.getInstance().removeMessageInterceptor(1);
			}
			else if (e.getSource() == deTranslationRB)
			{
				translatorInt.setDir(true);
				MessageDispatcher.getInstance().addMessageInterceptor(translatorInt, 1);
			}
			else if (e.getSource() == edTranslationRB)
			{
				translatorInt.setDir(false);
				MessageDispatcher.getInstance().addMessageInterceptor(translatorInt, 1);
			}
		}
	}
}