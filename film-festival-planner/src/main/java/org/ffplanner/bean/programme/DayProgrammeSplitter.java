package org.ffplanner.bean.programme;

import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayProgrammeSplitter {

    private final Collection<Showing> showings;

    private final Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours = new HashMap<>();

    private final SortedSet<Venue> venues;

    private Map<Integer, Showing> showingsByHour = new HashMap<>();

    private Venue venue;

    public DayProgrammeSplitter(Collection<Showing> showings, Comparator<Venue> venueComparator) {
        this.showings = showings;
        this.venues = new TreeSet<>(venueComparator);
        splitByVenueAndHour();
    }

    public SortedSet<Venue> getVenues() {
        return venues;
    }

    public Map<Venue, Map<Integer, Showing>> getShowingsByVenuesAndHours() {
        return showingsByVenuesAndHours;
    }

    private void splitByVenueAndHour() {
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
        if (!noCurrentVenue()) {
            showingsByVenuesAndHours.put(venue, showingsByHour);
            showingsByHour = new HashMap<>();
        }
    }
}
