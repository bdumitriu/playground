package jade;

import af.*;
import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.Agent;
import jade.core.Location;
import jade.domain.FIPANames;
import jade.domain.JADEAgentManagement.WhereIsAgentAction;
import jade.domain.mobility.MobilityOntology;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.sl.SL0Codec;
import jade.proto.FIPAProtocolNames;

import java.util.Iterator;

/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public abstract class JadeAgent extends Agent implements AFAgent
{
	private AFAgentID id = null;

	private Object location;

	protected void setup()
	{
		initialize();
	}

	public void addAFBehaviour(AFBehaviour behaviour)
	{
		behaviour.setAgent(this);
		addBehaviour(new JadeBehaviour(behaviour));
	}

	public void removeAFBehaviour(AFBehaviour behaviour)
	{
		behaviour.stop();
		// hopefully this will indeed remove the behaviour (since
		// the equals method has been overwritten in JadeBehaviour class)
		removeBehaviour(new JadeBehaviour(behaviour));
	}

	public AFMessage createEmptyMessage()
	{
		return new JadeMessage();
	}

	public void afSend(AFMessage message)
	{
		if (!(message instanceof JadeMessage))
			return;

		send(((JadeMessage) message).getMessage());
	}

	public AFMessage afReceive()
	{
		return new JadeMessage(receive());
	}

	public AFMessage afReceive(int type)
	{
		MessageTemplate mt = MessageTemplate.MatchPerformative(type);
		return new JadeMessage(receive(mt));
	}

	public AFMessage afBlockingReceive()
	{
		return new JadeMessage(blockingReceive());
	}

	public AFMessage afBlockingReceive(long millis)
	{
		return new JadeMessage(blockingReceive(millis));
	}

	public AFMessage afBlockingReceive(int type)
	{
		MessageTemplate mt = MessageTemplate.MatchPerformative(type);
		return new JadeMessage(blockingReceive(mt));
	}

	public AFMessage afBlockingReceive(long millis, int type)
	{
		MessageTemplate mt = MessageTemplate.MatchPerformative(type);
		return new JadeMessage(blockingReceive(mt, millis));
	}

	public AFAgentID getAgentID()
	{
		if (id == null)
		{
			id = new JadeAgentID(getName());
		}

		return id;
	}

	public void migrate(AFLocation location)
	{
		Location destination = ((JadeLocation) location).getLocation();
		doMove(destination);
	}

	public AFLocation getCurrentLocation()
	{
		if (getContentManager().lookupLanguage(FIPANames.ContentLanguage.FIPA_SL0) == null)
		{
			getContentManager().registerLanguage(new SLCodec(), FIPANames.ContentLanguage.FIPA_SL0);
			getContentManager().registerOntology(MobilityOntology.getInstance());
		}

		ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
		request.clearAllReceiver();

		request.clearAllReceiver();
		request.setProtocol(FIPAProtocolNames.FIPA_REQUEST);
		request.setOntology(MobilityOntology.NAME);
		request.setLanguage(SL0Codec.NAME);
		request.addReceiver(this.getAMS());

		try
		{
			Action action = new Action();
			action.setActor(this.getAMS());
			WhereIsAgentAction agAction = new WhereIsAgentAction();
			agAction.setAgentIdentifier(getAID());
			action.setAction(agAction);
			getContentManager().fillContent(request, action);
		}
		catch (Codec.CodecException e)
		{
			e.printStackTrace();
		}
		catch (OntologyException e)
		{
			e.printStackTrace();
		}

		send(request);

		MessageTemplate msgTmpl1 = MessageTemplate.MatchPerformative(ACLMessage.INFORM);
		MessageTemplate msgTmpl2 = MessageTemplate.MatchSender(getAMS());
		MessageTemplate msgTmpl = MessageTemplate.and(msgTmpl1, msgTmpl2);
		ACLMessage inform = blockingReceive(msgTmpl);

		try
		{
			Result results = (Result)
				getContentManager().extractContent(inform);
			Iterator iterator = results.getItems().iterator();
			location = iterator.next();
			if (!(location instanceof Location))
			{
				location = null;
			}
		}
		catch (Codec.CodecException e1)
		{
			e1.printStackTrace();
		}
		catch (OntologyException e1)
		{
			e1.printStackTrace();
		}

		if (location == null)
		{
			return null;
		}
		else
		{
			return new JadeLocation((Location) location);
		}
	}
}