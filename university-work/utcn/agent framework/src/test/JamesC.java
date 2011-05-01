package test;

import jade.JadeAgent;
import jade.JadeAgentID;
import af.ClientBehaviour;
import af.AFCommObject;
import af.AFClientServerJob;

/**
 * Created by IntelliJ IDEA.
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 5:47:17 PM
 * To change this template use Options | File Templates.
 */
public class JamesC extends JadeAgent
{
	public boolean initialize()
	{
		testClientBehaviour();
		return true;
	}

	private void testClientBehaviour()
	{
		DataContainer serverObj = new DataContainer();
		serverObj.setArg1(45);
		serverObj.setArg2(55);

		JadeAgentID serverID = new JadeAgentID("JamesS@omega:1099/JADE");

		ClientHandler clientHandler = new ClientHandler();

		addAFBehaviour(new ClientBehaviour(serverObj, serverID, clientHandler));
	}
}

class ClientHandler implements AFClientServerJob
{
	public AFCommObject process(AFCommObject obj)
	{
		DataContainer dc = (DataContainer) obj;
		System.out.println("Client got answer: " + dc.getRet());
		return null;
	}
}