package org.ffplanner.controller;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.inject.Named;
import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import org.ffplanner.ShowingEJB;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;

/**
 * @author Bogdan Dumitriu
 */
@Named(value = "showingController")
@RequestScoped
public class ShowingController implements Serializable {

    private static final SimpleDateFormat DAY_FORMAT = new SimpleDateFormat("dd/MM/yyyy");

    @EJB
    private ShowingEJB showingEJB;

    private Map<Venue, Map<Integer, Showing>> showingsByVenuesAndHours;

    private Collection<Venue> venues;

    private Collection<Hour> hours;

    private Date day;

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

    public String getPreviousDay() {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.roll(Calendar.DAY_OF_MONTH, false);
        return DAY_FORMAT.format(calendar.getTime());
    }

    public String getNextDay() {
        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.setTime(day);
        calendar.roll(Calendar.DAY_OF_MONTH, true);
        return DAY_FORMAT.format(calendar.getTime());
    }

    public void setDay(Date day) {
        this.day = day;
    }

    public String x() {
        return "index";
    }

    public String prepareShowings() {
        final ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
        final String day = externalContext.getRequestParameterMap().get("day");
        try {
            final Date date = DAY_FORMAT.parse(day);
            setDay(date);
        } catch (ParseException e) {
            e.printStackTrace();
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
        return "DaySchedule";
    }

    public static void main(String[] args) throws ParseException {
        final Date day = DAY_FORMAT.parse("03/06/2011");
        System.out.println(day);
    }
}
