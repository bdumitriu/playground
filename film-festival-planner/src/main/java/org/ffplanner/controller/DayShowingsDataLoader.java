package org.ffplanner.controller;

import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayShowingsDataLoader {

    private final Collection<Showing> showings;

    private final Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours = new HashMap<>();

    private final Collection<Venue> venues = new LinkedList<>();

    private Map<Integer, Showing> showingsByHour = new HashMap<>();

    private Venue venue;

    public DayShowingsDataLoader(Collection<Showing> showings) {
        this.showings = showings;
    }

    public Collection<Venue> getVenues() {
        return venues;
    }

    public Map<Venue, Map<Integer, Showing>> getShowingsByVenuesAndHours() {
        return showingsByVenuesAndHours;
    }

    public void load() {
        for (Showing showing : showings) {
            if (noCurrentVenue()) {
                changeVenueToThatOf(showing);
            } else if (venueChangeNecessary(showing)) {
                saveVenueShowings();
                changeVenueToThatOf(showing);
            }
            addShowing(showing);
        }
        saveVenueShowings();
    }

    private boolean noCurrentVenue() {
        return venue == null;
    }

    private void changeVenueToThatOf(Showing showing) {
        venue = showing.getVenue();
        venues.add(venue);
    }

    private boolean venueChangeNecessary(Showing showing) {
        return venue != showing.getVenue();
    }

    private void addShowing(Showing showing) {
        showingsByHour.put(showing.getHour(), showing);
    }

    private void saveVenueShowings() {
        showingsByVenuesAndHours.put(venue, showingsByHour);
        showingsByHour = new HashMap<>();
    }
}
