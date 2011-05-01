package geditor.repository;

/**
 * This exception is thrown whenever something went wrong with the repository (i.e. missing or corrupt version files).
 * <br /><br />
 * Date: Feb 26, 2004
 *
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class RepositoryException extends Exception
{

	public RepositoryException(String message)
	{
		super(message);
	}
}
