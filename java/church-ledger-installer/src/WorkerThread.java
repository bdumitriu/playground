
import java.util.Observable;

class WorkerThread extends Observable implements Runnable
{
	public WorkerThread(Worker[] workers)
	{
		this.workers = workers;
	}

	public void run()
	{
		for (int i = 0; i < workers.length; i++)
		{
			try
			{
				setChanged();
				notifyObservers(new Integer(i));
				workers[i].perform();
			}
			catch (Exception e)
			{
				System.out.println(e.getMessage());
				System.exit(1);
			}
		}
	}

	private Worker[] workers;
}