package org.ffplanner.entity;

import org.ffplanner.def.ScheduleDefinition;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public enum ScheduleConstraintType implements Serializable {

    // want to watch exactly this showing
    SHOWING(ScheduleDefinition.WatchType.SHOWING),

    // want to watch the movie, on this or some other showing
    MOVIE(ScheduleDefinition.WatchType.MOVIE),

    // watch this movie, if possible
    MAYBE_MOVIE(ScheduleDefinition.WatchType.MAYBE_MOVIE);

    private final ScheduleDefinition.WatchType watchType;

    ScheduleConstraintType(ScheduleDefinition.WatchType watchType) {
        this.watchType = watchType;
    }

    public ScheduleDefinition.WatchType getWatchType() {
        return watchType;
    }
}
