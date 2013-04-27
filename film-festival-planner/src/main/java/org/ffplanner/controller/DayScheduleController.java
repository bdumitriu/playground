package org.ffplanner.controller;

import org.ffplanner.bean.FestivalEditionBean;
import org.ffplanner.bean.programme.DayProgramme;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.converter.DayConverter;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.Venue;
import org.joda.time.DateTime;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.util.*;
import java.util.logging.Logger;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID;
import static org.joda.time.DateTimeConstants.JUNE;

/**
 * @author Bogdan Dumitriu
 */
@Named
@SessionScoped
public class DayScheduleController implements Serializable {

    private static final long serialVersionUID = 1L;

    private final Logger log = Logger.getLogger(DayScheduleController.class.getName());

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    @Inject
    private ScheduleController scheduleController;

    private FestivalEditionProgramme festivalProgramme;

    private final Hour[] hours;

    private Date day;

    private Long focusShowingId;

    private Showing showing;

    private DayProgramme dayProgramme;

    public DayScheduleController() {
        log.entering("DayScheduleController", "init");
        hours = Hour.hoursFrom(9, 2);
    }

    public void prepareView() {
//        log.setLevel(Level.ALL);
        log.entering("DayScheduleController", "prepareView");
        if (this.day == null) {
            final DateTime dateTime = new DateTime(2012, JUNE, 1, 0, 0);
            this.day = dateTime.toDate();
        }
        final FestivalEdition festivalEdition = festivalEditionBean.find(DEFAULT_FESTIVAL_EDITION_ID);
        festivalProgramme = festivalProgrammeBean.getProgrammeFor(festivalEdition);
        dayProgramme = festivalProgramme.getDayProgramme(this.day);

        scheduleController.updateConstraintsData();
        log.exiting("DayScheduleController", "prepareView");
    }

    public Hour[] getHours() {
        return hours;
    }

    public Collection<Venue> getVenues() {
        log.entering("DayScheduleController", "getVenues");
        final Collection<Venue> venues = dayProgramme.getVenues();
        try {
            return venues;
        } finally {
            log.exiting("DayScheduleController", "getVenues", venues == null ? "null" : venues.size());
        }
    }

    public Collection<HourSlot> getHourLineFor(Hour hour) {
        return dayProgramme.getHourLineFor(hour);
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
        log.entering("DayScheduleController", "setDay", day == null ? null : day.toString());
        this.day = day;
        log.exiting("DayScheduleController", "setDay");
    }

    public Date getDay() {
        return day;
    }

    public void setFocusShowingId(Long focusShowingId) {
        this.focusShowingId = focusShowingId;
    }

    public Long getFocusShowingId() {
        return focusShowingId;
    }

    public void setShowing(Long showingId) {
        this.showing = festivalProgramme.getShowingFor(showingId);
    }

    public Showing getShowing() {
        return showing;
    }

    public Showing[] getOtherShowings() {
        if (showing != null) {
            final List<Showing> showings = festivalProgramme.getShowingsForSameMovieAs(showing);
            final Set<Showing> otherShowings = new TreeSet<>();
            for (Showing similarShowing : showings) {
                if (!similarShowing.equals(showing)) {
                    otherShowings.add(similarShowing);
                }
            }
            return otherShowings.toArray(new Showing[otherShowings.size()]);
        } else {
            return new Showing[0];
        }
    }

    public String getShowingDay(Showing showing) {
        return new DayConverter().getAsString(null, null, showing.getDateAndTime());
    }
}
