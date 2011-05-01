package test;

import jade.JadeAgent;
import jade.lang.acl.ACLMessage;
import af.*;

/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 22, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class TestAgent extends JadeAgent
{
	public ListenerBehaviour listBeh;

	public boolean initialize()
	{
		TestEvent event = new TestEvent();

		listBeh = new ListenerBehaviour(this, ACLMessage.QUERY_REF, event);
		addAFBehaviour(listBeh);

		return true;
	}

	public void stopMe()
	{
		removeAFBehaviour(listBeh);
	}
}

class TestEvent implements ListenerEvent
{
	public void callBackWhenReceived(AFMessage message)
	{
		System.out.println("received message with type " + message.getType() + " and with content:");
		System.out.println(message.getContent());
	}
}