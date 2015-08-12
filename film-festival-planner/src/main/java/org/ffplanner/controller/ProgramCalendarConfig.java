package org.ffplanner.controller;

import javax.inject.Inject;

/**
 * @author Bogdan Dumitriu
 */
public class ProgramCalendarConfig extends DefaultCalendarConfig {

    @Inject
    private ScheduleController scheduleController;

    @Override
    public String getMovieStyleClass(Long showingId) {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("sch_movie_cell");
        if (scheduleController.isWatchShowingSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_showing");
        } else if (scheduleController.isWatchMovieSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_movie");
        } else if (scheduleController.isWatchShowingSelectedElsewhere(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_elsewhere");
        }
        return stringBuilder.toString();
    }

    @Override
    public boolean showWatchAnyFor(Long showingId) {
        return scheduleController.hasNoConstraintForShowing(showingId);
    }

    @Override
    public boolean showRemoveInterestFor(Long showingId) {
        return scheduleController.hasUserConstraintFor(showingId);
    }

    @Override
    public boolean showPriorityControlFor(Long showingId) {
        return scheduleController.hasUserConstraintFor(showingId);
    }

    @Override
    public boolean showWatchThisFor(Long showingId) {
        return scheduleController.hasWeakConstraintFor(showingId);
    }

    @Override
    public boolean showWatchThisSelectedByUserFor(Long showingId) {
        return scheduleController.isWatchShowingSelected(showingId);
    }

    @Override
    public boolean showWatchThisSelectedBySystemFor(Long showingId) {
        return false;
    }
}
