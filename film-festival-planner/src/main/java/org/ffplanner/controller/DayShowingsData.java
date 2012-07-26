package org.ffplanner.controller;

import org.ffplanner.bean.ShowingEJB;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayShowingsData {

    private final ShowingEJB showingEJB;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    public DayShowingsData(ShowingEJB showingEJB) {
        this.showingEJB = showingEJB;
    }

    public void loadFor(Date day) {
        final Collection<Showing> showings = showingEJB.getShowingsFor(day);
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
    }

    public Collection<Venue> getVenues() {
        return venues;
    }

    public Map<Integer, Showing> getShowingsByHoursFor(Venue venue) {
        return showingsByVenuesAndHours.get(venue);
    }
}
