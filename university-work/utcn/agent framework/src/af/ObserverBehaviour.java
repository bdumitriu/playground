package af;

import java.util.ArrayList;

/**
 * This is a continuous behaviour which is used by agents needing to periodically check
 * for the occurence of certain events and take actions when these events occur. In order
 * to temporarily discontinue the observation you should call the {@link #suspend} method.
 * To resume observation, call the {@link #resume} method. If you want to permanently
 * discontinue the observation, remove this behaviour from the agent.
 *
 * Date: May 22, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class ObserverBehaviour extends SingleBehaviour
{
	private ArrayList data;
	private boolean startCalled;
	private long timeUnit;

	/**
	 * Builds a new ObserverBehaviour which uses millisecond as its time unit.
	 */
	public ObserverBehaviour()
	{
		super();
		this.timeUnit = 1;
		data = new ArrayList();
		startCalled = false;
	}

	/**
	 * Builds a new ObserverBehaviour.
	 *
         * @param timeUnit the time unit (in milliseconds) used as a base for all
	 *      periods required in the {@link #registerObserverEvent} method.
	 */
	public ObserverBehaviour(long timeUnit)
	{
		super();
		this.timeUnit = timeUnit;
		data = new ArrayList();
		startCalled = false;
	}

	/**
	 * Register the event whose occurence this behaviour should check for periodically.
	 * All events you are interested in should be added using this method *before* adding
	 * this behaviour to an agent. If you call this method afterwards, it will simply return
	 * without achieving any effect.
	 *
	 * @param event an ObeserverEvent object defining the event to observe and
	 *      the action to take when event occurs.
	 * @param period the period (in the time units you build this behavior with) which
	 *      defines how often the occurence of the event should be checked. The actual
	 *      period is computed as period * timeUnit (the resulting period representing
	 *      milliseconds).
	 */
	public void registerObserverEvent(ObserverEvent event, long period)
	{
		if (startCalled)
		{
			return;
		}

		ObserverData od = new ObserverData(null, event, period * timeUnit);
		data.add(od);
	}

	/**
	 * This is the method called by the timer class when a timer event
	 * occurs.
	 */
	protected void timerEventOccured(int index)
	{
		if (started)
		{
			ObserverEvent event = ((ObserverData) data.get(index)).event;
			if (event.hasOccured())
			{
				event.callBackWhenOccured();
			}
		}
	}

	public boolean start()
	{
		if (!startCalled)
		{
			startCalled = true;
		}
		else
		{
			return true;
		}

		int size = data.size();
		for (int i = 0; i < size; i++)
		{
			ObserverData od = (ObserverData) data.get(i);
			if (od.timer != null)
			{
				od.timer.destroy();
				try
				{
					od.timer.join();
				}
				catch (InterruptedException e)
				{}
			}
			od.timer = new Timer(od.period, this, i);
			od.timer.start();
		}

		started = true;

		return true;
	}

	public void suspend()
	{
		started = false;
	}

	public void resume()
	{
		started = true;
	}

	public void stop()
	{
		started = false;
		startCalled = false;

		int size = data.size();
		for (int i = 0; i < size; i++)
		{
			ObserverData od = (ObserverData) data.get(i);

			if (od.timer != null)
			{
				od.timer.destroy();
			}
		}

		// wait for all threads to finish execution
		try
		{
			for (int i = 0; i < size; i++)
			{
				ObserverData od = (ObserverData) data.get(i);
				od.timer.join();
				od.timer = null;
			}
		}
		catch (InterruptedException e)
		{}
	}

	/*
	 * Data container class.
	 */
	class ObserverData
	{
		public Timer timer;
		public ObserverEvent event;
		public long period;

		public ObserverData(Timer timer, ObserverEvent event, long period)
		{
			this.timer = timer;
			this.event = event;
			this.period = period;
		}
	}
}

class Timer extends Thread
{
	private ObserverBehaviour handler;
	private long period;
	private int index;
	private boolean die;

	public Timer(long period, ObserverBehaviour handler, int index)
	{
		this.handler = handler;
		this.period = period;
		this.index = index;
		die = false;
	}

	public void destroy()
	{
		die = true;
		interrupt();
	}

	public void run()
	{
		while (!die)
		{
			try
			{
				sleep(period);
			}
			catch (InterruptedException e)
			{}

			if (!die)
			{
				handler.timerEventOccured(index);
			}
		}
	}
}