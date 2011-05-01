package gw.render;

/**
 * This Exception can be thrown when a Stylesheet is created
 */
public final class StylesheetCreateException extends Exception {

    /**
     * Constructor which calls the super
     */
    public StylesheetCreateException() {
        super();
    }

    /**
     * Constructor which calls the super with the given message
     * 
     * @param message
     *            The message which describes the Exception
     */
    public StylesheetCreateException(String message) {
        super(message);
    }

    /**
     * Constructor which calls the super with the given cause
     * 
     * @param cause
     *            The cause of the RegisterException
     */
    public StylesheetCreateException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructor which calls the super with the given message and cause
     * 
     * @param message
     *            The message which describes the Exception
     * @param cause
     *            The cause of the RegisterException
     */
    public StylesheetCreateException(String message, Throwable cause) {
        super(message, cause);
    }
}
