
import java.util.Observable;
import java.util.Properties;

/**
 * Base class for all workers.
 *
 * @author Bogdan Dumitriu
 * @version 0.1
 */
public abstract class Worker extends Observable
{
	/**
	 * Creates a new Worker with no configuration and with and with a percentile delta (see
	 * {@link #setCallbackFrequency(byte) setCallbackFrequency} for details) set to 1.
	 */
	public Worker()
	{
		this(null);
	}

	/**
	 * Creates a new Worker with the supplied <code>config</code>uration and with a percentile delta (see
	 * {@link #setCallbackFrequency(byte) setCallbackFrequency} for details) set to 1.
	 *
	 * @param config the Properties to use in order to get configuration data
	 */
	public Worker(Properties config)
	{
		this.config = config;
		this.percentile = 1;
	}

	/**
	 * Sets the percentile delta to be used as a callback frequency measure. The observers will be called back at
	 * 0% completed, <code>perDelta</code>% completed, 2*<code>>perDelta</code>% completed, ..., 100% completed.
	 * If <code>perDelta</code> is lower than 1 or higher than 100, it will be set to 100 by default.
	 */
	public void setCallbackFrequency(byte perDelta)
	{
		if ((perDelta < 1) || (perDelta > 100))
		{
			percentile = 100;
		}
		else
		{
			percentile = perDelta;
		}
	}

	/**
	 * Sets a description for this worker.
	 */
	public void setDescription(String description)
	{
		this.description = description;
	}

	/**
	 * Returns the description for the worker.
	 */
	public String getDescription()
	{
		return description;
	}

	/**
	 * Sets an order number for this worker.
	 */
	public void setWorkerNr(int workerNr)
	{
		this.workerNr = workerNr;
	}

	/**
	 * Returns the number of this worker.
	 */
	public int getWorkerNr()
	{
		return workerNr;
	}

	/**
	 * Performs the actual work this worker is meant to do.
	 *
	 * @throws Exception if something goes wrong and a message has to be displayed to the user. The getMessage()
	 *	method of the Exception will contain a user-ready error message.
	 */
	public abstract void perform() throws Exception;

	protected Properties config;

	protected byte percentile;

	private String description;
	private int workerNr;
}
