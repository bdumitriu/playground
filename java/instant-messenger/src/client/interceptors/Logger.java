package client.interceptors;

import client.contexts.*;

import javax.swing.*;
import java.awt.*;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 20, 2005
 */
public class Logger extends JFrame implements LoginInterceptor, FriendInterceptor, MessageInterceptor
{
	public Logger()
	{
		super("Logger");

		textArea = new JTextArea(5, 50);
		textArea.setEnabled(false);
		textArea.setDisabledTextColor(Color.BLACK);
		JScrollPane jsp = new JScrollPane(textArea);
		add(jsp);
		pack();
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
	}

	private void log(String message)
	{
		textArea.append(message + "\n");
		textArea.setCaretPosition(textArea.getText().length());
	}

	public void preLogin(LoginContext ctx)
	{}

	public void postLogin(LoginContext ctx)
	{
		if (ctx.getOutcome() == Outcome.FAILED)
		{
			log("Login of " + ctx.getUserName() + " failed.");
		}
		else
		{
			log("Login of " + ctx.getUserName() + " succeeded.");
		}
	}

	public void preLogout(LoginContext ctx)
	{}

	public void postLogout(LoginContext ctx)
	{
		log(ctx.getUserName() + " logged out.");
	}

	public void friendAdded(FriendContext ctx)
	{
		log(ctx.getUserName() + " added " + ctx.getFriendName() + " as a friend.");
	}

	public void friendRemoved(FriendContext ctx)
	{}

	public void wentOnline(FriendContext ctx)
	{
		log(ctx.getFriendName() + " has logged in.");
	}

	public void wentOffline(FriendContext ctx)
	{
		log(ctx.getFriendName() + " has logged out.");
	}

	public void receivedFriendsList(FriendListContext ctx)
	{
		log("Friends list received.");
	}

	public void preMessageSend(MessageContext ctx)
	{}

	public void messageSent(MessageContext ctx)
	{
		if (ctx.getOutcome() == MessageOutcome.NOT_ONLINE)
		{
			log("Message <" + ctx.getMessage() + "> to " + ctx.getToUser() + " was not sent because " +
				ctx.getToUser() + " is not online.");
		}
		else if (ctx.getOutcome() == MessageOutcome.OK)
		{
			log("Message <" + ctx.getMessage() + "> sent to " + ctx.getToUser() + ".");
		}
	}

	public void messageReceived(MessageContext ctx)
	{
		log("Message <" + ctx.getMessage() + "> received from " + ctx.getFromUser() + ".");
	}

	JTextArea textArea;
}
