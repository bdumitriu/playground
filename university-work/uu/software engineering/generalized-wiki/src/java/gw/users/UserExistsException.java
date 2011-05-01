package gw.users;


/**
 * Exception thrown if a user does exist.
 */
public class UserExistsException extends RuntimeException {

	public UserExistsException( final String message ) {
		
		super( message );
	}
}
