package gw.render.parsers;

/**
 * This Exception is thrown by a Parser
 */
public class ParseException extends Exception {
	
	/**
	 * Constructor which calls the super with the given Exception
	 * 
	 * @param e The Exception which is the cause of this ParseException
	 */
	public ParseException(Exception e) {

		super(e);
	}
	
}
