package org.ffplanner.controller;

import org.ffplanner.bean.ScheduleBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.bean.UserBean;
import org.ffplanner.converter.DayConverter;
import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.User;
import org.ffplanner.entity.Venue;
import org.ffplanner.qualifier.LoggedInUser;
import org.joda.time.DateTime;

import javax.ejb.EJB;
import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.Serializable;
import java.util.*;
import java.util.logging.Logger;

import static org.joda.time.DateTimeConstants.JUNE;

/**
 * @author Bogdan Dumitriu
 */
@Named
@SessionScoped
public class DayScheduleController implements Serializable {

    private static final long serialVersionUID = 868645927823933930L;

    private final Logger log = Logger.getLogger(DayScheduleController.class.getName());

    @Inject @LoggedInUser
    private User user;

    @EJB
    private UserBean userBean;

    @EJB
    private ScheduleBean scheduleBean;

    @EJB
    private ShowingBean showingBean;

    private final Collection<Hour> hours;

    private Date day;

    private DayShowingsData dayShowingsData;

    private ConstraintsData constraintsData;

    public DayScheduleController() {
        log.entering("DayScheduleController", "init");
        hours = new ArrayList<>();
        for (int i = 9; i < 24; i++) {
            hours.add(new Hour(i));
        }
        hours.add(new Hour(0));
        hours.add(new Hour(1));
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
        dayShowingsData = new DayShowingsData(showingBean);
        dayShowingsData.loadFor(this.day, this.hours);
        System.out.println("Loading Constraints Data.....................................");
        constraintsData = new ConstraintsData(userBean);
        constraintsData.loadFor(this.user);
        log.exiting("DayScheduleController", "prepareView");
    }

    public Collection<Hour> getHours() {
        return hours;
    }

    public Collection<Venue> getVenues() {
        log.entering("DayScheduleController", "getVenues");
        final Collection<Venue> venues = dayShowingsData.getVenues();
        try {
            return venues;
        } finally {
            log.exiting("DayScheduleController", "getVenues", venues == null ? "null" : venues.size());
        }
    }

    public Collection<HourSlot> getHourLineFor(Hour hour) {
        return dayShowingsData.getHourLineFor(hour);
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
        scheduleBean.toggleAnyConstraint(showingId, user.getId(), ScheduleConstraintType.SHOWING);
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
        scheduleBean.toggleConstraint(showingId, user.getId(), constraintType);
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
