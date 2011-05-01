package af;

/**
 * Created by IntelliJ IDEA.
 *
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 1:57:21 PM
 * To change this template use Options | File Templates.
 */
public class ClientBehaviour extends SingleBehaviour
{
	private AFCommObject commObject;

	private AFAgentID agentID;

	private AFClientServerJob handler;

	private static int oneSecond = 1000;
	private static int waitPeriod = 10 * oneSecond;

	private boolean startCalled;

	public ClientBehaviour(AFCommObject commObject, AFAgentID agentID,
	        AFClientServerJob handler)
	{
		super();
		this.commObject = commObject;
		this.agentID = agentID;
		this.handler = handler;

		startCalled = false;
	}

	public boolean start()
	{
		if (!startCalled)
		{
			startCalled = true;
		}
		else
		{
			return false;
		}

		AFMessage message = getAgent().createEmptyMessage();
		message.setType(AFMessage.AF_REQUEST);
		message.setObjectContent(commObject);
		message.addReceiver(agentID);
		message.setSender(getAgent().getAgentID());

		// send initial request
		getAgent().afSend(message);

		// await the positive acknowledge
		AFMessage ack = getAgent().afBlockingReceive(waitPeriod, AFMessage.AF_ACKNOWLEDGE);
		if (ack.isNull())
		{
			handler.process(null);
			hasRun = true;
			return false;
		}

		AFMessage reply = getAgent().afBlockingReceive(AFMessage.AF_REPLY);
		if (reply.isNull())
		{
			handler.process(null);
			hasRun = true;
			return false;
		}

		Object content = reply.getObjectContent();

		if ((content == null) || (! (content instanceof AFCommObject)))
		{
			handler.process(null);
			hasRun = true;
			return false;
		}

		handler.process((AFCommObject) content);

		hasRun = true;

		return true;
	}

	public boolean canBeRestarted()
	{
		return false;
	}
}