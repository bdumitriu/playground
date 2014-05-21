package firstcup.webservice;

import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriInfo;

@Path("dukesAge")
public class DukesAgeResource {

	@SuppressWarnings("unused")
	@Context
	private UriInfo context;

	/**
	 * Default constructor.
	 */
	public DukesAgeResource() {

	}

	/**
	 * Retrieves representation of an instance of DukesAgeResource
	 *
	 * @return an instance of String
	 */
	@GET
	@Produces(MediaType.TEXT_PLAIN)
	public String getText() {
		// Create a new Calendar for Duke's birthday
		Calendar dukesBirthday = new GregorianCalendar(1995, Calendar.MAY, 23);
		// Create a new Calendar for today
		Calendar now = Calendar.getInstance();

		// Subtract today's year from Duke's birth year, 1995
		int dukesAge = now.get(Calendar.YEAR) - dukesBirthday.get(Calendar.YEAR);
		dukesBirthday.add(Calendar.YEAR, dukesAge);

		// If today's date is before May 23, subtract a year from Duke's age
		if (now.before(dukesBirthday)) {
			dukesAge--;
		}
		// Return a String representation of Duke's age
		return new String("" + dukesAge);
	}
}
