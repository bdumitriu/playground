package gw.render;

/**
 * This Exception can be thrown when applying a Stylesheet
 */
public final class StylesheetApplyException extends Exception {

    /**
     * Constructor which calls the super
     */
    public StylesheetApplyException() {
        super();
    }

    /**
     * Constructor which calls the super with the given message
     * 
     * @param message
     *            The message which describes the Exception
     */
    public StylesheetApplyException(String message) {
        super(message);
    }

    /**
     * Constructor which calls the super with the given cause
     * 
     * @param cause
     *            The cause of the RegisterException
     */
    public StylesheetApplyException(Throwable cause) {
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
    public StylesheetApplyException(String message, Throwable cause) {
        super(message, cause);
    }
}
