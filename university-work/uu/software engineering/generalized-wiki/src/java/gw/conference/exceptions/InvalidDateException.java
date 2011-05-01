/*
 * Created on Oct 4, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package gw.conference.exceptions;

public class InvalidDateException extends ConferenceException {

	private static final long serialVersionUID = 8585;

	public InvalidDateException (String date) {
		super("This date (" + date +") is not in a valid form, dd-mm-yyyy");
	}
}
