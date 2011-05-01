package gw.conference.exceptions;

public class ConferenceExistsException extends ConferenceException {
    private static final long serialVersionUID = 5529195380140921376L;
    
    public ConferenceExistsException() {
        super("A conference already exists at the specified location.");
    }
}
