package gw.conference.exceptions;


public class ReviewExistsException extends ConferenceException {
	private static final long serialVersionUID = 1265732;

    public ReviewExistsException(String message) {
        super(message);
    }
}
