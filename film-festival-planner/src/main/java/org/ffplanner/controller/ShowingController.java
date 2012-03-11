package org.ffplanner.controller;

import org.ffplanner.bean.ShowingEJB;
import org.ffplanner.converter.DayConverter;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;
import org.joda.time.DateTime;

import javax.ejb.EJB;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import java.io.Serializable;
import java.util.*;
import java.util.logging.Logger;

import static org.joda.time.DateTimeConstants.JUNE;

/**
 * @author Bogdan Dumitriu
 */
@ManagedBean
@ViewScoped
public class ShowingController implements Serializable {

    private static final long serialVersionUID = 868645927823933930L;

    private final Logger log = Logger.getLogger(ShowingController.class.getName());

    @EJB
    private ShowingEJB showingEJB;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    private final Collection<Hour> hours;

    private Date day;

    public ShowingController() {
        log.entering("ShowingController", "init");
        hours = new ArrayList<>();
        for (int i = 9; i < 24; i++) {
            hours.add(new Hour(i));
        }
        hours.add(new Hour(0));
        hours.add(new Hour(1));
    }

    public void prepareShowings() {
//        log.setLevel(Level.ALL);
        log.entering("ShowingController", "prepareShowings");
        if (this.day == null) {
            final DateTime dateTime = new DateTime(2011, JUNE, 3, 0, 0);
            this.day = dateTime.toDate();
        }
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
        log.exiting("ShowingController", "prepareShowings");
    }

    public Collection<Hour> getHours() {
        return hours;
    }

    public Collection<Venue> getVenues() {
        log.entering("ShowingController", "getVenues");
        try {
            return venues;
        } finally {
            log.exiting("ShowingController", "getVenues", venues == null ? "null" : venues.size());
        }
    }

    public Collection<HourSlot> getHourSlotsFor(Venue venue) {
        log.entering("ShowingController", "getHourSlotsFor", venue == null ? "null" : venue.getName());
        final Map<Integer, Showing> showingsByHour = showingsByVenuesAndHours.get(venue);
        final Collection<HourSlot> hourSlots = new LinkedList<>();
        for (Hour hour : getHours()) {
            final Showing showing = showingsByHour == null ? null : showingsByHour.get(hour.getHour());
            hourSlots.add(new HourSlot(hour, showing));
        }
        try {
            return hourSlots;
        } finally {
            log.exiting("ShowingController", "getHourSlotsFor");
        }
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
        log.entering("ShowingController", "setDay", day == null ? null : day.toString());
        this.day = day;
        log.exiting("ShowingController", "setDay");
    }

    public Date getDay() {
        return day;
    }

    public void showingClicked(Long showingId) {

    }
}
