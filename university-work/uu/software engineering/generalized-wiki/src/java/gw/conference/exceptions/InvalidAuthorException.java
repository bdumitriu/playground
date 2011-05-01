package gw.conference.exceptions;


public class InvalidAuthorException extends ConferenceException {
	private static final long serialVersionUID = -6268762710343772345L;

    public InvalidAuthorException(String authorName) {
		super(authorName + " is not a valid author.");
	}
}
