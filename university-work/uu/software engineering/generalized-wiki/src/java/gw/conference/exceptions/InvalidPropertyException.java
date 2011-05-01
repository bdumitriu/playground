package gw.conference.exceptions;

public class InvalidPropertyException extends ConferenceException {
    private static final long serialVersionUID = -7027266758272844296L;

    public InvalidPropertyException(String property) {
        super("The property '" + property + "' is not a valid property for this.");
    }
}
