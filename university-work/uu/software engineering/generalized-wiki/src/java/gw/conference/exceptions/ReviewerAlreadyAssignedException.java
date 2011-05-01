package gw.conference.exceptions;

public class ReviewerAlreadyAssignedException extends ConferenceException {
    private static final long serialVersionUID = 87238787;
    
    public ReviewerAlreadyAssignedException(String message) {
        super(message);
    }
}