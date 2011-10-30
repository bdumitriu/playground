package org.ffplanner.controller;

import java.util.Collection;
import java.util.Date;
import javax.inject.Named;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import org.ffplanner.ShowingEJB;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

/**
 *
 * @author Bogdan Dumitriu
 */
@Named(value = "showingController")
@RequestScoped
public class ShowingController implements Serializable {

	@EJB
	private ShowingEJB showingEJB;

	private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

	private Collection<Venue> venues;

	private Collection<Hour> hours;

	public ShowingController() {
		hours = new ArrayList<>();
		for (int i = 9; i < 24; i++) {
			hours.add(new Hour(i));
		}
		for (int i = 0; i < 2; i++) {
			hours.add(new Hour(i));
		}
	}

	public Collection<Hour> getHours() {
		return hours;
	}

	public Collection<Venue> getVenues() {
		return venues;
	}

	public Collection<HourSlot> getHourSlotsFor(Venue venue) {
		final Map<Integer, Showing> showingsByHour = showingsByVenuesAndHours.get(venue);
		final LinkedList<HourSlot> hourSlots = new LinkedList<>();
		for (Hour hour : getHours()) {
			final Showing showing = showingsByHour == null ? null : showingsByHour.get(hour.getHour());
			hourSlots.add(new HourSlot(hour, showing));
		}
		return hourSlots;
	}

	public String prepareShowings() {
		final GregorianCalendar calendar = new GregorianCalendar(2011, Calendar.JUNE, 3);
		final Collection<Showing> showings = showingEJB.getShowingsFor(calendar.getTime());
		showingsByVenuesAndHours = new HashMap<>();
		venues = new LinkedList<>();
		Map<Integer, Showing> showingsByHour = new HashMap<>();
		Venue currentVenue = null;
		for (Showing showing : showings) {
			if (currentVenue == null) {
				currentVenue = showing.getVenue();
				venues.add(currentVenue);
			} else if (currentVenue != showing.getVenue()) {
				showingsByVenuesAndHours.put(currentVenue, showingsByHour);
				currentVenue = showing.getVenue();
				venues.add(currentVenue);
				showingsByHour = new HashMap<>();
			}
			final Calendar dateAndTime = new GregorianCalendar();
			dateAndTime.setTime(showing.getDateAndTime());
			showingsByHour.put(dateAndTime.get(Calendar.HOUR_OF_DAY), showing);
		}
		showingsByVenuesAndHours.put(currentVenue, showingsByHour);
		return "DaySchedule";
	}
}
