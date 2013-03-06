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
import java.util.*;
import java.util.logging.Logger;

import static org.ffplanner.entity.ScheduleConstraintType.MOVIE;
import static org.ffplanner.entity.ScheduleConstraintType.SHOWING;
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

    private int priority;

    private DayProgramme dayProgramme;

    private ConstraintsData constraintsData;

    private List<Long> scheduledShowings;

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

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public void movieCellClicked(Long showingId) {
        userScheduleBean.toggleAnyConstraint(showingId, user.getId(), MOVIE);
        /* TODO: temporary */
        if (hasSchedule()) {
            scheduledShowings.remove(showingId);
        }
    }

    public void watchThisButtonClicked(Long showingId) {
        userScheduleBean.toggleFallbackConstraint(showingId, user.getId(), SHOWING, MOVIE);
    }

    public void watchAnyButtonClicked(Long showingId) {
        showingButtonClicked(showingId, MOVIE);
    }

    public void maybeWatchButtonClicked(Long showingId) {
        showingButtonClicked(showingId, ScheduleConstraintType.MAYBE_MOVIE);
    }

    public void assignPriority(Long showingId) {
        // TODO: save priority
    }

    private void showingButtonClicked(Long showingId, ScheduleConstraintType constraintType) {
        userScheduleBean.toggleConstraint(showingId, user.getId(), constraintType);
    }

    public boolean hasSchedule() {
        return scheduledShowings != null;
    }

    public void suggestSchedule() {
        final ScheduleBuilder scheduleBuilder = festivalProgrammeBean.getScheduleBuilder(DEFAULT_FESTIVAL_EDITION_ID);
        final Schedule schedule = scheduleBuilder.getPossibleSchedulesJ(constraintsData.asScheduleDefinition()).get(0);
        scheduledShowings = new LinkedList<>();
        scheduledShowings.addAll(schedule.showingsJ());
    }

    public void discardSchedule() {
        scheduledShowings = null;
    }

    public boolean hasConstraints() {
        return constraintsData.size() > 0;
    }

    public String getMovieStyleClass(Long showingId) {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("sch_movie_cell");
        if (isWatchThisSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_this");
        } else if (isScheduled(showingId)) {
            stringBuilder.append(" sch_movie_cell_scheduled");
        } else if (isWatchAnySelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_any");
        } else if (isMaybeWatchSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_maybe_watch");
        }
        return stringBuilder.toString();
    }

    public boolean isScheduled(Long showingId) {
        return hasSchedule() && scheduledShowings.contains(showingId);
    }

    public boolean isWatchThisSelected(Long showingId) {
        return isConstraintSelected(showingId, SHOWING);
    }

    public boolean isSomethingOtherThanWatchThisSelected(Long showingId) {
        return isDifferentConstraintSelected(showingId, SHOWING);
    }

    public boolean isWatchAnySelected(Long showingId) {
        return isConstraintSelected(showingId, MOVIE);
    }

    public boolean isSomethingOtherThanWatchAnySelected(Long showingId) {
        return isDifferentConstraintSelected(showingId, MOVIE);
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
