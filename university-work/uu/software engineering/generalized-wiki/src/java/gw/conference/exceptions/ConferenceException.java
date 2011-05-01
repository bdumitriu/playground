package gw.conference.exceptions;

/** 
 * Exception base class for all exceptions thrown by the Conference application.
 * All subclasses must specify a message (that may be displayed by servlets).
 *  */
public abstract class ConferenceException extends Exception {
    public ConferenceException(String message, Exception cause) {
        super(message, cause);
    }
    public ConferenceException(String message) {
        super(message);
    }
}