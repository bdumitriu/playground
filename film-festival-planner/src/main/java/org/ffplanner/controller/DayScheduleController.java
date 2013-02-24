package org.ffplanner.controller;

import org.ffplanner.Schedule;
import org.ffplanner.ScheduleBuilder;
import org.ffplanner.bean.FestivalEditionBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.bean.programme.DayProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.converter.DayConverter;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.User;
import org.ffplanner.entity.Venue;
import org.ffplanner.qualifier.LoggedInUser;
import org.joda.time.DateTime;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
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

    @Inject @LoggedInUser
    private User user;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    @Inject
    private UserScheduleBean userScheduleBean;

    @Inject
    private ShowingBean showingBean;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    private final Hour[] hours;

    private Date day;

    private DayProgramme dayProgramme;

    private ConstraintsData constraintsData;

    private Schedule schedule;

    public DayScheduleController() {
        log.entering("DayScheduleController", "init");
        hours = Hour.hoursFrom(9, 2);
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public void prepareView() {
//        log.setLevel(Level.ALL);
        log.entering("DayScheduleController", "prepareView");
        if (this.day == null) {
            final DateTime dateTime = new DateTime(2012, JUNE, 1, 0, 0);
            this.day = dateTime.toDate();
        }
        final FestivalEdition festivalEdition = festivalEditionBean.find(DEFAULT_FESTIVAL_EDITION_ID);
        dayProgramme = festivalProgrammeBean.getProgrammeFor(festivalEdition).getDayProgramme(this.day);

        if (constraintsData == null) {
            constraintsData = new ConstraintsData(userScheduleBean, festivalEdition);
            constraintsData.loadFor(this.user);
        } else if (constraintsData.reloadNeeded()) {
            constraintsData.loadFor(this.user);
        }

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

    public void movieCellClicked(Long showingId) {
        userScheduleBean.toggleAnyConstraint(showingId, user.getId(), ScheduleConstraintType.MOVIE);
    }

    public void watchThisButtonClicked(Long showingId) {
        showingButtonClicked(showingId, ScheduleConstraintType.SHOWING);
    }

    public void watchAnyButtonClicked(Long showingId) {
        showingButtonClicked(showingId, ScheduleConstraintType.MOVIE);
    }

    public void maybeWatchButtonClicked(Long showingId) {
        showingButtonClicked(showingId, ScheduleConstraintType.MAYBE_MOVIE);
    }

    private void showingButtonClicked(Long showingId, ScheduleConstraintType constraintType) {
        userScheduleBean.toggleConstraint(showingId, user.getId(), constraintType);
    }

    public boolean hasSchedule() {
        return schedule != null;
    }

    public void suggestSchedule() {
        final ScheduleBuilder scheduleBuilder = festivalProgrammeBean.getScheduleBuilder(DEFAULT_FESTIVAL_EDITION_ID);
        schedule = scheduleBuilder.getPossibleSchedulesJ(constraintsData.asScheduleDefinition()).get(0);
    }

    public void acceptSchedule() {
        // ???
    }

    public void discardSchedule() {
        schedule = null;
    }

    public boolean hasConstraints() {
        return constraintsData.size() > 0;
    }

    public boolean isScheduled(Long showingId) {
        return hasSchedule() && schedule.showingsJ().contains(showingId);
    }

    public boolean isWatchThisSelected(Long showingId) {
        return isConstraintSelected(showingId, ScheduleConstraintType.SHOWING);
    }

    public boolean isSomethingOtherThanWatchThisSelected(Long showingId) {
        return isDifferentConstraintSelected(showingId, ScheduleConstraintType.SHOWING);
    }

    public boolean isWatchAnySelected(Long showingId) {
        return isConstraintSelected(showingId, ScheduleConstraintType.MOVIE);
    }

    public boolean isSomethingOtherThanWatchAnySelected(Long showingId) {
        return isDifferentConstraintSelected(showingId, ScheduleConstraintType.MOVIE);
    }

    public boolean isMaybeWatchSelected(Long showingId) {
        return isConstraintSelected(showingId, ScheduleConstraintType.MAYBE_MOVIE);
    }

    public boolean isSomethingOtherThanMaybeWatchSelected(Long showingId) {
        return isDifferentConstraintSelected(showingId, ScheduleConstraintType.MAYBE_MOVIE);
    }

    private boolean isConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        return constraintsData.isConstraintSelected(showingId, constraintType);
    }

    private boolean isDifferentConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        return constraintsData.isDifferentConstraintSelected(showingId, constraintType);
    }

    public boolean isAnyConstraintSelectedFor(Long showingId) {
        return constraintsData.isAnyConstraintSelected(showingId);
    }
}
