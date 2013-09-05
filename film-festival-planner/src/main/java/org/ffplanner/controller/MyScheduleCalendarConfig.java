package org.ffplanner.controller;

import javax.inject.Inject;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class MyScheduleCalendarConfig extends DefaultCalendarConfig implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private ScheduleController scheduleController;

    @Override
    public boolean showMovieId(Long showingId) {
        return scheduleController.isScheduled(showingId);
    }
}
