package gw.users;

/**
 * Exception thrown when a asked user does not exists on a certain place.
 */
public class NoSuchUserException extends RuntimeException {

	/**
	 * Create a NoSuchUserException with a given message.
	 * @param message the exceptionmessage
	 */
	public NoSuchUserException( final String message ) {
		
		super( message );
	}
}
