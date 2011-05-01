package af;

/**
 * The class is ment to wrap around an exception raised if the time
 * interval for an objective object is invalid (negative time cuanta or
 * earliest greater than highest).
 *
 * Date: 26.05.2003
 * Time: 12:00:18
 * @author Tudor Marian,
 * @author email tudorm@coned.utcluj.ro
 * @version 0.1
 */
public class IllegalTimeIntervalException extends Exception
{
	public IllegalTimeIntervalException(String message)
	{
		super(message);
	}
}
