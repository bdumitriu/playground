package jade;

import af.AFAgentID;
import af.AFMessage;
import jade.core.AID;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.UnreadableException;
import jade.util.leap.Iterator;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;


/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */

public class JadeMessage implements AFMessage, Serializable
{
	private ACLMessage message;

	public JadeMessage()
	{
		message = new ACLMessage(ACLMessage.NOT_UNDERSTOOD);
	}

	public JadeMessage(ACLMessage message)
	{
		this.message = message;
	}

	public void setContent(String content)
	{
		if (message != null)
		{
			message.setContent(content);
		}
	}

	public String getContent()
	{
		if (message != null)
		{
			return message.getContent();
		}

		return null;
	}

	public boolean setObjectContent(Serializable content)
	{
		if (content == null)
			return false;

		try
		{
			message.setContentObject(content);
		}
		catch (IOException e)
		{
			return false;
		}

		return true;
	}

	public Serializable getObjectContent()
	{
		Serializable retVal = null;

		try
		{
			retVal = message.getContentObject();
		}
		catch (UnreadableException e)
		{}

		return retVal;
	}

	public void setSender(AFAgentID sender)
	{
		if (!(sender instanceof JadeAgentID))
			return;

		if (message != null)
		{
			message.setSender(new AID(sender.getID(), true));
		}
	}

	public AFAgentID getSender()
	{
		AID aid = null;
		if (message != null)
		{
			aid = message.getSender();
		}

		if (aid != null)
		{
			return new JadeAgentID(aid.getName());
		}
		else
		{
			return new JadeAgentID("");
		}
	}

	public void addReceiver(AFAgentID receiver)
	{
		if (!(receiver instanceof JadeAgentID))
			return;

		if (message != null)
		{
			message.addReceiver(new AID(receiver.getID(), true));
		}
	}

	public void removeReceiver(AFAgentID receiver)
	{
		if (!(receiver instanceof JadeAgentID))
			return;

		if (message != null)
		{
			message.removeReceiver(new AID(receiver.getID(), true));
		}
	}

	public AFAgentID[] getReceivers()
	{
		if (message == null)
		{
			return null;
		}

		ArrayList aids = new ArrayList();
		Iterator it = message.getAllReceiver();

		while (it.hasNext())
		{
			aids.add(it.next());
		}

		return (AFAgentID[]) aids.toArray();
	}

	public void setType(int type)
	{
		if (message != null)
		{
			message.setPerformative(type);
		}
	}

	public int getType()
	{
		if (message != null)
		{
			return message.getPerformative();
		}

		return -1;
	}

	public boolean isNull()
	{
		return (message == null);
	}

	public ACLMessage getMessage()
	{
		return message;
	}
}