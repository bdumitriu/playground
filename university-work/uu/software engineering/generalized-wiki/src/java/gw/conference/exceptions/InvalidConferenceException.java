package gw.conference.exceptions;

public class InvalidConferenceException extends ConferenceException {
    private static final long serialVersionUID = 13265136;
    
    public InvalidConferenceException(String message, Exception cause) {
        super(message, cause);
    }
    public InvalidConferenceException(String message) {
        super(message);
    }
}