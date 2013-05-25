package org.ffplanner.bean.programme;

import org.ffplanner.helper.Hour;
import org.ffplanner.helper.HourSlot;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayProgramme {

    private static final DayProgramme EMPTY_PROGRAMME = new DayProgramme(Collections.<Showing>emptyList(), null);

    private final Collection<Showing> showings;

    private final Comparator<Venue> venueComparator;

    private final Map<Hour, Collection<HourSlot>> hourSlots;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    /**
     * @param showings
     *             all of a day's showings, expected in venue-sorted order
     */
    public DayProgramme(Collection<Showing> showings, Comparator<Venue> venueComparator) {
        this.showings = showings;
        this.venueComparator = venueComparator;
        this.hourSlots = new EnumMap<>(Hour.class);
        splitByVenueAndHour();
        createHourSlots();
    }

    public static DayProgramme getEmptyProgramme() {
        return EMPTY_PROGRAMME;
    }

    public Collection<Showing> getShowings() {
        return showings;
    }

    public Collection<Venue> getVenues() {
        return venues;
    }

    public Map<Integer, Showing> getShowingsByHoursFor(Venue venue) {
        return showingsByVenuesAndHours.get(venue);
    }

    public Collection<HourSlot> getHourLineFor(Hour hour) {
        return hourSlots.get(hour);
    }

    private void splitByVenueAndHour() {
        final DayProgrammeSplitter dayProgrammeSplitter = new DayProgrammeSplitter(showings, venueComparator);
        showingsByVenuesAndHours = dayProgrammeSplitter.getShowingsByVenuesAndHours();
        venues = new LinkedList<>(dayProgrammeSplitter.getVenues());
    }

    private void createHourSlots() {
        for (Hour hour : Hour.allHoursInDay()) {
            hourSlots.put(hour, createHourLineFor(hour));
        }
    }

    private Collection<HourSlot> createHourLineFor(Hour hour) {
        final Collection<HourSlot> hourLine = new LinkedList<>();
        for (Venue venue : getVenues()) {
            final Map<Integer, Showing> showingsByHour = getShowingsByHoursFor(venue);
            final Showing showing = showingsByHour == null ? null : showingsByHour.get(hour.getHour());
            hourLine.add(new HourSlot(hour, showing));
        }
        return hourLine;
    }
}
