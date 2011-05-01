
package gw.render;

/**
 * This Exception is thrown by the StylesheetFactory
 */
public class RegisterException extends RuntimeException {

	/**
	 * Constructor which calls the super
	 */
	public RegisterException() {
		super();
	}

	/**
	 * Constructor which calls the super with the given message
	 * 
	 * @param message The message which describes the Exception
	 */
	public RegisterException(String message) {
		super(message);
	}

	/**
	 * Constructor which calls the super with the given cause
	 * 
	 * @param cause The cause of the RegisterException
	 */
	public RegisterException(Throwable cause) {
		super(cause);
	}

	/**
	 * Constructor which calls the super with the given message and cause
	 * 
	 * @param message The message which describes the Exception
	 * @param cause The cause of the RegisterException
	 */
	public RegisterException(String message, Throwable cause) {
		super(message, cause);
	}
}
