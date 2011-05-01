package gw.conference.exceptions;


public class AuthorExistsException extends ConferenceException {
	private static final long serialVersionUID = 1254125;
	
	public AuthorExistsException(String authorName) {
		super("Author " + authorName + " already exists.");
	}
}
