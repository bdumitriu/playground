package gw.conference.exceptions;

public class InvalidPaperException extends ConferenceException {
    private static final long serialVersionUID = 91871287;
    
    public InvalidPaperException(String message) {
        super(message);
    }
}