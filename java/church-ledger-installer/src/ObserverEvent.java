
/**
 * Created by IntelliJ IDEA.
 * User: bdumitriu
 * Date: Feb 17, 2005
 * Time: 10:50:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class ObserverEvent
{
	public ObserverEvent(int workerNr, int percentile)
	{
		this.workerNr = workerNr;
		this.percentile = percentile;
	}

	public int getWorkerNr()
	{
		return workerNr;
	}

	public int getPercentile()
	{
		return percentile;
	}

	private int workerNr;
	private int percentile;
}
