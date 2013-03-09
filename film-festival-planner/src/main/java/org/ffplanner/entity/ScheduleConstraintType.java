package org.ffplanner.entity;

import org.ffplanner.def.ConstraintDefinition.WatchType;

import java.io.Serializable;
import java.util.EnumSet;

/**
 * @author Bogdan Dumitriu
 */
public enum ScheduleConstraintType implements Serializable {

    /** want to watch exactly this showing */
    SHOWING(WatchType.SHOWING),

    /** want to watch the movie, on this or some other showing */
    MOVIE(WatchType.MOVIE),

    /** a SHOWING constraint is set for this movie elsewhere */
    SHOWING_ELSEWHERE(WatchType.MOVIE);

    /** the constraints that are set by the user directly (as opposed to computed by the system) */
    public static final EnumSet<ScheduleConstraintType> USER_CONSTRAINTS = EnumSet.of(SHOWING, MOVIE);

    /** the constraints that can still be changed to a stronger one */
    public static final EnumSet<ScheduleConstraintType> WEAK_CONSTRAINTS = EnumSet.complementOf(EnumSet.of(SHOWING));

    private final WatchType watchType;

    ScheduleConstraintType(WatchType watchType) {
        this.watchType = watchType;
    }

    public WatchType getWatchType() {
        return watchType;
    }
}
