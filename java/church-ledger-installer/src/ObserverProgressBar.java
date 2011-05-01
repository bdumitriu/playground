
import javax.swing.*;
import java.util.Observer;
import java.util.Observable;

/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Jan 10, 2005
 * Time: 12:32:11 AM
 * To change this template use File | Settings | File Templates.
 */
public class ObserverProgressBar extends JProgressBar implements Observer
{
	public ObserverProgressBar(int nrWorkers)
	{
		super();
		interval = (double) ((double) 100 / (double) nrWorkers);
		this.nrWorkers = nrWorkers;
	}

	public ObserverProgressBar(int orient, int nrWorkers)
	{
		super(orient);
		interval = (double) ((double) 100 / (double) nrWorkers);
		this.nrWorkers = nrWorkers;
	}

	public ObserverProgressBar(int min, int max, int nrWorkers)
	{
		super(min, max);
		interval = (double) ((double) 100 / (double) nrWorkers);
		this.nrWorkers = nrWorkers;
	}

	public ObserverProgressBar(int orient, int min, int max, int nrWorkers)
	{
		super(orient, min, max);
		interval = (double) ((double) 100 / (double) nrWorkers);
		this.nrWorkers = nrWorkers;
	}

	public ObserverProgressBar(BoundedRangeModel newModel, int nrWorkers)
	{
		super(newModel);
		interval = (double) ((double) 100 / (double) nrWorkers);
		this.nrWorkers = nrWorkers;
	}

	public void update(Observable o, Object arg)
	{
		int percentile = ((ObserverEvent) arg).getPercentile();
		int workerNr = ((ObserverEvent) arg).getWorkerNr();

		double begin = (double) ((double) workerNr * interval);
		double value = (double) ((double) ((double) percentile * interval) / (double) 100);
		value += begin;

		setValue((int) value);

		if (nrWorkers == workerNr + 1 && percentile == 100)
		{
			setValue(100);
		}
	}

	int nrWorkers;
	double interval;
}
