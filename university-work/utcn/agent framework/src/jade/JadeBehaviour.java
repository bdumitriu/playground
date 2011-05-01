package jade;

import af.AFBehaviour;
import jade.core.behaviours.Behaviour;

/**
 * Created by IntelliJ IDEA.
 *
 * Date: May 19, 2003
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class JadeBehaviour extends Behaviour
{
	private AFBehaviour behaviour;

	public JadeBehaviour(AFBehaviour behaviour)
	{
		this.behaviour = behaviour;
	}

	public void action()
	{
		behaviour.start();
	}

	public void block()
	{
		super.block();
		behaviour.suspend();
	}

	public void restart()
	{
		super.restart();
		behaviour.resume();
	}

	public boolean done()
	{
		return behaviour.hasRun() && !behaviour.canBeRestarted();
	}

	/**
	 * Two JadeBehaviour's are equal if their underlying AFBehaviour's are equal.
	 */
	public boolean equals(Object o)
	{
		if (this == o) return true;
		if (!(o instanceof JadeBehaviour)) return false;

		final JadeBehaviour jadeBehaviour = (JadeBehaviour) o;

		if (behaviour != null ? !behaviour.equals(jadeBehaviour.behaviour) : jadeBehaviour.behaviour != null) return false;

		return true;
	}

	public int hashCode()
	{
		return (behaviour != null ? behaviour.hashCode() : 0);
	}
}