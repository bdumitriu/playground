package geditor.repository;

import java.rmi.RemoteException;

/**
 * This exception is thrown when the version parameters of the {@link Repository#getOperations(int, int)} getOperations}
 * have faulty values.
 * <br /><br />
 * Date: Feb 26, 2004
 * 
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitriu.ro
 * @version 0.1
 */
public class VersionException extends Exception
{
	public VersionException(String message)
	{
		super(message);
	}
}
