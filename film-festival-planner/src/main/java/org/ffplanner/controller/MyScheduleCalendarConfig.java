package org.ffplanner.controller;

import javax.inject.Inject;

/**
 * @author Bogdan Dumitriu
 */
public class MyScheduleCalendarConfig extends DefaultCalendarConfig {

    @Inject
    private ScheduleController scheduleController;

    @Override
    public boolean showMovieId(Long showingId) {
        return scheduleController.isScheduled(showingId) || scheduleController.couldNotBeScheduled(showingId);
    }

    @Override
    public String getMovieStyleClass(Long showingId) {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("sch_movie_cell");
        if (scheduleController.isWatchShowingSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_showing");
        } else if (scheduleController.isScheduled(showingId)) {
            stringBuilder.append(" sch_movie_cell_scheduled");
        } else if (scheduleController.isScheduledElsewhere(showingId)
                || scheduleController.isWatchShowingSelectedElsewhere(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_elsewhere");
        } else if (scheduleController.isWatchMovieSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_movie");
        }
        return stringBuilder.toString();
    }

    @Override
    public boolean showWatchAnyFor(Long showingId) {
        return false;
    }

    @Override
    public boolean showRemoveInterestFor(Long showingId) {
        return false;
    }

    @Override
    public boolean showPriorityControlFor(Long showingId) {
        return false;
    }

    @Override
    public boolean showWatchThisFor(Long showingId) {
        return scheduleController.couldNotBeScheduled(showingId);
    }

    @Override
    public boolean showWatchThisSelectedByUserFor(Long showingId) {
        return false;
    }

    @Override
    public boolean showWatchThisSelectedBySystemFor(Long showingId) {
        return scheduleController.isScheduled(showingId);
    }
}
