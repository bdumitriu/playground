package org.ffplanner.controller;

import org.ffplanner.ShowingEJB;
import org.ffplanner.converter.DayConverter;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import java.io.Serializable;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@ManagedBean
@RequestScoped
public class ShowingController implements Serializable {

    @EJB
    private ShowingEJB showingEJB;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    private final Collection<Hour> hours;

    private Date day;

    public ShowingController() {
        hours = new ArrayList<>();
        for (int i = 9; i < 24; i++) {
            hours.add(new Hour(i));
        }
        hours.add(new Hour(0));
        hours.add(new Hour(1));
    }

    public void prepareShowings() {
        final Collection<Showing> showings = showingEJB.getShowingsFor(this.day);
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

    public Collection<Hour> getHours() {
        return hours;
    }

    public Collection<Venue> getVenues() {
        return venues;
    }

    public Collection<HourSlot> getHourSlotsFor(Venue venue) {
        final Map<Integer, Showing> showingsByHour = showingsByVenuesAndHours.get(venue);
        final Collection<HourSlot> hourSlots = new LinkedList<>();
        for (Hour hour : getHours()) {
            final Showing showing = showingsByHour == null ? null : showingsByHour.get(hour.getHour());
            hourSlots.add(new HourSlot(hour, showing));
        }
        return hourSlots;
    }

    public String getPreviousDay() {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.roll(Calendar.DAY_OF_MONTH, false);
        return new DayConverter().getAsString(null, null, calendar.getTime());
    }

    public String getNextDay() {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.roll(Calendar.DAY_OF_MONTH, true);
        return new DayConverter().getAsString(null, null, calendar.getTime());
    }

    public void setDay(Date day) {
        this.day = day;
    }

    public Date getDay() {
        return day;
    }
}
