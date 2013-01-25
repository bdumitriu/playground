package org.ffplanner.controller;

import org.ffplanner.bean.ShowingBean;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
public class DayShowingsData {

    private final ShowingBean showingBean;

    private Map<Hour, Collection<HourSlot>> hourSlots;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    public DayShowingsData(ShowingBean showingBean) {
        this.showingBean = showingBean;
    }

    public void loadFor(Date day, Iterable<Hour> hours) {
        final Collection<Showing> showings = showingBean.getShowingsFor(day);
        final DayShowingsDataLoader dayShowingsDataLoader = new DayShowingsDataLoader(showings);
        dayShowingsDataLoader.load();
        showingsByVenuesAndHours = dayShowingsDataLoader.getShowingsByVenuesAndHours();
        venues = dayShowingsDataLoader.getVenues();
        hourSlots = createHourSlots(hours);
    }

    private Map<Hour, Collection<HourSlot>> createHourSlots(Iterable<Hour> hours) {
        final Map<Hour, Collection<HourSlot>> hourSlots = new HashMap<>();
        for (Hour hour : hours) {
            hourSlots.put(hour, createHourLineFor(hour));
        }
        return hourSlots;
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

    public Collection<Venue> getVenues() {
        return venues;
    }

    public Map<Integer, Showing> getShowingsByHoursFor(Venue venue) {
        return showingsByVenuesAndHours.get(venue);
    }

    public Collection<HourSlot> getHourLineFor(Hour hour) {
        return hourSlots.get(hour);
    }
}
