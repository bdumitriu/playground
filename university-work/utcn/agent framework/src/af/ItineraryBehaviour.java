package af;

import java.util.*;

/**
 * The class represents an itinerary based behavior. An agent is given
 * an itinerary and the coresponding time based constraints and it is
 * the agent's job to reach the destinations on time. If one (or many)
 * destinations are not reached on time, an <code>ObjectiveMissedBehaviour
 * </code> is called upon which may choose the handling policy (may stop
 * or even may continue).
 *
 * Date: 26.05.2003
 * Time: 09:53:29
 * @author Tudor Marian,
 * @author email tudorm@coned.utcluj.ro
 * @version 0.1
 */

public class ItineraryBehaviour extends SingleBehaviour
{
	/**
	 * This parameter represents the granularity of the period in millis.
	 * The time units are multiplied by this parameter to obtain a value
	 * in milliseconds.
	 */
	long timePeriod;

	/**
	 * This is the list of objectives, it may not be modified. Once this
	 * behaviour has been added it cannot be modified.
	 */
	private List objectives;

	/**
	 * The behaviour called upon if a location is not reached within
	 * the time bounds.
	 */
	private ObjectiveMissedBehaviour missBehaviour;

	/**
	 * Not supposed to call the start method of this behaviour twice,
	 * this is a watch dog.
	 */
	private boolean startCalled;

	/**
	 * The runner thread. This separate thread is the one that keeps the
	 * management of the time-awareness ability of the agent. It is required
	 * because of the timing policy chosen.
	 */
	//private Thread runner;

	/**
	 * The time reference from start of this behavior.
	 */
	private long timeBase;

	/**
	 * The current objective.
	 */
	private Objective currentObjective;

	/**
	 * The route this agent has passed through.
	 */
	//private List route;

	/**
	 * The objectives this agent has missed.
	 */
	private List missedObjectives;

	/**
	 * While this parameter is false, the agent's time quanta monitor
	 * thread executes, else the thread will be stopped.
	 */
	//private boolean endRunner;

	/**
	 * This attribute signals if the object has migrated and it's state is
	 * to be restored.
	 */
	//private boolean toBeRestored;

	/**
	 * The time (in milliseconds) estimation for agent migration from one
	 * place to another.
	 */
	private long migrationDelay;

	/**
	 * The listeners registered to be called upon when a new objective is reached.
	 */
	private List listeners;

	/**
	 * The constructor.
	 *
	 * @param timePeriod    the time period in milliseconds
	 * @param objectives    the list of objectives
	 */
	public ItineraryBehaviour(long timePeriod, long migrationDelay, List objectives)
	{
		super();
		this.timePeriod = timePeriod;
		this.migrationDelay = migrationDelay;
		this.objectives = objectives;

		init();
	}

	/**
	 * The constructor.
	 *
	 * @param timePeriod    the time period in millisecinds
	 * @param objectives    the list of objectives
	 * @param missBehaviour the behaviour to be called upon one miss
	 */
	public ItineraryBehaviour(long timePeriod, long migrationDelay, List objectives,
	                          ObjectiveMissedBehaviour missBehaviour)
	{
		super();
		this.timePeriod = timePeriod;
		this.migrationDelay = migrationDelay;
		this.objectives = objectives;
		this.missBehaviour = missBehaviour;

		init();
	}

	/**
	 * Private method to initialize all needed attributes.
	 */
	private void init()
	{
		prepareObjectives();

		startCalled = false;

		timeBase = 0;

		missedObjectives = new LinkedList();

		if (missBehaviour != null)
		{
			missBehaviour.setContextBehaviour(this);
		}

		listeners = new LinkedList();
	}

	/**
	 * Setter method.
	 */
	public void setMissBehaviour(ObjectiveMissedBehaviour missBehaviour)
	{
		this.missBehaviour = missBehaviour;
		missBehaviour.setContextBehaviour(this);
	}

	/**
	 * Register a listener object to be notified upon arrival at a new objective.
	 * @param listener      The listener that should be notified
	 */
	public void addObjectiveReachedListener(ObjectiveReachedListener listener)
	{
		if (listeners != null)
		{
			listeners.add(listener);
		}
	}

	/**
	 * Unregister a listener object, it will stop being notified upon arrival at a
	 * new objective.
	 * @param listener      The listener that will not be notified any more
	 */
	public void removeObjectiveReachedListener(ObjectiveReachedListener listener)
	{
		if (listeners != null)
		{
			listeners.remove(listener);
		}
	}

	/**
	 * This method is called upon the arrival at a new objective an it will notify
	 * all the listeners.
	 * @param event         The event object sent with the notification
	 */
	private void fireEvent(ObjectiveReachedEvent event)
	{
		if (listeners != null)
		{
			Iterator iterator = listeners.iterator();

			while (iterator.hasNext())
			{
				((ObjectiveReachedListener) iterator.next()).callBackWhenReached(event);
			}
		}
	}

	/**
	 * The method just prepares the list of objectives sorting them.
	 */
	private void prepareObjectives()
	{
		if (objectives == null)
			return;

		if (objectives.isEmpty())
		{
			return;
		}

		// an empty objective used to sort the partial destinations
		Comparator comparator = new Objective();
		Collections.sort(objectives, comparator);
	}

	public void stop()
	{
		started = false;
		hasRun = true;
	}

	public void suspend()
	{
		started = false;
	}

	public void resume()
	{
		started = true;
	}

	public boolean canBeRestarted()
	{
		return false;
	}

	public boolean start()
	{
		if (! startCalled)
		{
			startCalled = true;
		}
		else
		{
			return true;
		}

		while (!objectives.isEmpty())
		{
			currentObjective = (Objective) objectives.get(0);
			if (getAgent().getCurrentLocation().equals(currentObjective.getLocation()))
			{
				objectives.remove(0);
				if (timeBase < currentObjective.getEarliest() * timePeriod)
				{
					try
					{
						System.out.println("going to sleep at timeBase " + timeBase + "...");
						Thread.sleep(currentObjective.getEarliest() * timePeriod - timeBase);
						timeBase = currentObjective.getEarliest() * timePeriod;
					}
					catch (InterruptedException e)
					{
						// insomniac thread...
						e.printStackTrace();
					}
				}

				if (timeBase > currentObjective.getLatest() * timePeriod)
				{
					// add up the missed address
					missedObjectives.add(currentObjective);

					if (missBehaviour == null)
					{
						Objective objective;
						boolean flag = true;
						while (flag)
						{
							objective = (Objective) objectives.get(0);

							if ((objective.getLatest() * timePeriod) < 
								(timeBase + migrationDelay))
							{
								missedObjectives.add(objectives.remove(0));
							}
							else
							{
								flag = false;
							}
						}
					}
					else
					{
						getAgent().addAFBehaviour(missBehaviour);
					}
				}
				else
				{
					ObjectiveReachedEvent event = new 
						ObjectiveReachedEvent(timeBase, currentObjective);
					fireEvent(event);
				}
			}
			else
			{
				// save state - mark the behavior to be called again
				startCalled = false;

				// move self
				try
				{
					System.out.println("migrating at timeBase " + timeBase + "...");
					getAgent().migrate(currentObjective.getLocation());
					timeBase += migrationDelay;
					return true;
				}
				catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		}

		hasRun = true;
		return true;
	}

	/*
	public boolean start()
	{
		if ((!toBeRestored) && (startCalled))
		{
			return true;
		}

		startCalled = true;

		if (!super.start())
			return false;

		started = true;

		restoreState();

		while (!objectives.isEmpty())
		{
			System.out.println("HERE");
			currentObjective = (Objective) objectives.remove(0);

			// wait until the agent is supposed to migrate
			synchronized (this)
			{
				while (currentObjective.getEarliest() > timeBase)
				{
					try
					{
						this.wait();
					}
					catch (InterruptedException e)
					{
						e.printStackTrace();
					}
				}
			}

			// if I missed this add another behavior
			if (currentObjective.getLatest() > timeBase)
			{
				// add up the missed address
				missedObjectives.add(currentObjective.getLocation());

				if (missBehaviour == null)
				{
					// default policy if no missBeahaviour is given: just stop
					stop();
				}
				else
				{
					getAgent().addAFBehaviour(missBehaviour);
				}
			}
			else
			{
				// add up the location to the route
				route.add(currentObjective.getLocation());

				// save self's state
				saveState();
				// move self
				//getAgent().migrate(currentObjective.getLocation());
			}
		}

		// in the end destroy the time quanta thread
		destroyTimeQuantaThread();

		// do not allow the agent to respawn
		toBeRestored = false;

		System.out.println("DONE");


		return true;
	}

	*/

	/**
	 * The method saves the agent's state, the agent is supposed to be
	 * on the brink of migration.
	 */
/*	private void saveState()
	{
		// terminate the time quanta thread
		destroyTimeQuantaThread();

		// mark the fact that the agent is supose to resume it's state
		toBeRestored = true;

		// mark the behavior to be called again
		startCalled = false;
	}
*/
	/**
	 * The method restores the agent's state upon arrival to another location.
	 */
/*	private void restoreState()
	{
		endRunner = false;

		runner = new Thread(this);

		runner.start();
	}
*/
	/**
	 * The method destroys the time quanta thread. Usefull when the agent
	 * is about to migrate to another location and must restore it's state.
	 */
/*	private void destroyTimeQuantaThread()
	{
		endRunner = true;

		// wait for the runner to finish
		try
		{
			runner.join();
		}
		catch (InterruptedException e)
		{}

		// prepare thread to be garbage collected
		runner = null;
	}

	// the runner method for the counter thread
	public void run()
	{
		while (!endRunner)
		{
			try
			{
				// if the time is near, wake up the main thread to do the
				// checkings
				if (currentObjective != null)
				{
					if (currentObjective.getEarliest() <= (timeBase++))
					{
						synchronized (this)
						{
							this.notify();
						}
					}
				}

				Thread.sleep(timePeriod);
			}
			catch (InterruptedException e)
			{
				System.out.println("quanta thread interrupted");
			}
		}
	}
*/
}
