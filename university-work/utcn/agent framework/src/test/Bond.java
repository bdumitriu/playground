package test;

import af.ItineraryBehaviour;
import af.Objective;
import af.ObjectiveReachedEvent;
import af.ObjectiveReachedListener;
import jade.JadeAgent;
import jade.JadeLocation;
import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.Location;
import jade.domain.FIPANames;
import jade.domain.JADEAgentManagement.QueryPlatformLocationsAction;
import jade.domain.mobility.MobilityOntology;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.lang.sl.SL0Codec;
import jade.proto.FIPAProtocolNames;

import java.util.Iterator;
import java.util.LinkedList;


/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 22, 2003
 * @author Bogdan Dumitriu, Tudor Marian
 * @version 0.1
 */
public class Bond extends JadeAgent
{
	public ItineraryBehaviour itinerary;
	public LinkedList route = new LinkedList();

	private void testItineraryBehaviour()
	{
		StandardInineraryListener il = new StandardInineraryListener();
		getContainerLocations();
		itinerary = new ItineraryBehaviour(1000, 500, route);
		itinerary.addObjectiveReachedListener(il);
		addAFBehaviour(itinerary);
	}

	public boolean initialize()
	{
		testItineraryBehaviour();
		return true;
	}

	private void getContainerLocations()
	{
		// requiered stuff
		// register the SL0 content language
		if (getContentManager().
		        lookupLanguage(FIPANames.ContentLanguage.FIPA_SL0) == null)
		{
			getContentManager().registerLanguage(new SLCodec(),
			        FIPANames.ContentLanguage.FIPA_SL0);
			// register the mobility ontology
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
			action.setAction(new QueryPlatformLocationsAction());
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

		MessageTemplate msgTmpl1 =
		        MessageTemplate.MatchPerformative(ACLMessage.INFORM);
		MessageTemplate msgTmpl2 = MessageTemplate.MatchSender(getAMS());
		MessageTemplate msgTmpl = MessageTemplate.and(msgTmpl1, msgTmpl2);
		ACLMessage inform = blockingReceive(msgTmpl);

		try
		{
			Result results = (Result)
				getContentManager().extractContent(inform);

			Iterator iterator = results.getItems().iterator();

			Object location;

			int i = 1;

			while (iterator.hasNext())
			{
				location = iterator.next();

				if (location instanceof Location)
				{
					route.add(
						new Objective(new JadeLocation
						        ((Location)location), i*2, 3*i
						)
					);
				}
				else
				{
					System.out.println("Incompatible");
				}

				i++;
			}
		}
		catch (Exception e)
		{}
	}
}

class StandardInineraryListener implements ObjectiveReachedListener
{
	public void callBackWhenReached(ObjectiveReachedEvent event)
	{
		System.out.println("At objective: " +
		        ((JadeLocation)event.getObjective().getLocation()).getLocation() +
		                " and timebase: " + event.getTimeBase());
	}

}