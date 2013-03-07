package org.ffplanner.entity;

import org.ffplanner.def.ConstraintDefinition.WatchType;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public enum ScheduleConstraintType implements Serializable {

    // want to watch exactly this showing
    SHOWING(WatchType.SHOWING),

    // want to watch the movie, on this or some other showing
    MOVIE(WatchType.MOVIE);

    private final WatchType watchType;

    ScheduleConstraintType(WatchType watchType) {
        this.watchType = watchType;
    }

    public WatchType getWatchType() {
        return watchType;
    }
}
