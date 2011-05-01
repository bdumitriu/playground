package test;

import jade.JadeAgent;
import jade.JadeAgentID;
import af.ClientBehaviour;
import af.AFCommObject;
import af.AFClientServerJob;
import af.ServerBehaviour;

/**
 * Created by IntelliJ IDEA.
 * User: tudorm
 * Date: Aug 21, 2003
 * Time: 5:47:17 PM
 * To change this template use Options | File Templates.
 */
public class JamesS extends JadeAgent
{
	public boolean initialize()
	{
		testServerBehaviour();
		return true;
	}

	private void testServerBehaviour()
	{
		ServerHandler serverHandler = new ServerHandler();

		addAFBehaviour(new ServerBehaviour(serverHandler));
	}
}

class ServerHandler implements AFClientServerJob
{
	public AFCommObject process(AFCommObject obj)
	{
		DataContainer dc = (DataContainer) obj;
		dc.setRet(dc.getArg1() + dc.getArg2());
		return dc;
	}
}