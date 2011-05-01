package gw.conference.exceptions;

public class PaperExistsException extends ConferenceException {
	private static final long serialVersionUID = 4373477;
	
	public PaperExistsException(String paperName) {
		super("Paper " + paperName + " already exists.");
	}
}