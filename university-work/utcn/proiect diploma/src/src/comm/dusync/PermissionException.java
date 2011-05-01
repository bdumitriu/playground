package comm.dusync;

/**
 * This exception indicates that the remote user doesn't want to allow the requesting user to synchronize.
 */
public class PermissionException extends Exception
{
	public PermissionException(String msg)
	{
		super(msg);
	}
}
