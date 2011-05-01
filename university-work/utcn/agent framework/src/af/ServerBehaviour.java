package af;

import java.util.LinkedList;

/**
 * Created by IntelliJ IDEA.
 *
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 1:57:21 PM
 * To change this template use Options | File Templates.
 */
public class ServerBehaviour extends SingleBehaviour
{
	private AFClientServerJob handler;

	private boolean startCalled;

	private LinkedList servers;

	private boolean stillServing;

	public ServerBehaviour(AFClientServerJob handler)
	{
		super();
		this.handler = handler;
		startCalled = false;

		stillServing = true;

		servers = new LinkedList();
	}

	public boolean start()
	{
		AFMessage message;
		ServerWorker worker;

		while (stillServing)
		{
			message = getAgent().afBlockingReceive(AFMessage.AF_REQUEST);
			if (message.isNull())
				continue;

			worker = new ServerWorker(message, handler, getAgent());
			servers.add(worker);
			worker.start();
		}

		while (!(servers.isEmpty()))
		{
			try
			{
				((ServerWorker) servers.removeFirst()).join();
			}
			catch (InterruptedException e)
			{
				return false;
			}
		}

		hasRun = true;

		return true;
	}

	public void stop()
	{
		stillServing = false;
		hasRun = true;
	}

	public boolean canBeRestarted()
	{
		return false;
	}
}

class ServerWorker extends Thread
{
	private AFMessage initMessage;
	private AFClientServerJob handler;
	private AFAgent agent;

	public ServerWorker(AFMessage initMessage, AFClientServerJob handler, AFAgent agent)
	{
		this.initMessage = initMessage;
		this.handler = handler;
		this.agent = agent;
	}

	public void run()
	{
		AFAgentID clientID = initMessage.getSender();

		// send acknowledge
		AFMessage ack = agent.createEmptyMessage();
		ack.setType(AFMessage.AF_ACKNOWLEDGE);
		ack.addReceiver(clientID);
		ack.setSender(agent.getAgentID());

		agent.afSend(ack);

		// perform the job

		Object obj = initMessage.getObjectContent();

		if ((obj == null) || (! (obj instanceof AFCommObject)))
		{
			return;
		}

		AFCommObject commObj = handler.process((AFCommObject) obj);

		if (commObj == null)
		{
			return;
		}

		// send back the reply
		AFMessage reply = agent.createEmptyMessage();
		reply.setType(AFMessage.AF_REPLY);
		reply.setObjectContent(commObj);
		reply.addReceiver(clientID);
		reply.setSender(agent.getAgentID());

		agent.afSend(reply);
	}
}
