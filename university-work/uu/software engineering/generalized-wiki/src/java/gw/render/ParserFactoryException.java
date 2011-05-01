package gw.render;

/**
 * This Exception is thrown by the ParserFactory
 */
public class ParserFactoryException extends Exception {

	/**
	 * Constructor which calls the super with the given message
	 * 
	 * @param message The message which describes the Exception
	 */
	public ParserFactoryException(String message) {
		super(message);
	}
}
