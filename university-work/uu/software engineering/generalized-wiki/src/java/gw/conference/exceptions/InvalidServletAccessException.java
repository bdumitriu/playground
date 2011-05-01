package gw.conference.exceptions;

/** Thrown when a servlet is accessed incorrectly (e.g. GET request or no parameters). */
public class InvalidServletAccessException extends ConferenceException {
    private static final long serialVersionUID = -1874749579237923190L;

    public InvalidServletAccessException() {
        super("Invalid call to this servlet. Please go to the root of this conference, and follow the instructions.");
    }
}
