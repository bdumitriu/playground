package org.ffplanner.entity;

import java.io.Serializable;
import java.util.EnumSet;

/**
 * @author Bogdan Dumitriu
 */
public enum ScheduleConstraintType implements Serializable {

    /** want to watch exactly this showing */
    SHOWING,

    /** want to watch the movie, on this or some other showing */
    MOVIE,

    /** a SHOWING constraint is set for this movie elsewhere */
    SHOWING_ELSEWHERE;

    /** the constraints that are set by the user directly (as opposed to computed by the system) */
    public static final EnumSet<ScheduleConstraintType> USER_CONSTRAINTS = EnumSet.of(SHOWING, MOVIE);

    /** the constraints that can still be changed to a stronger one */
    public static final EnumSet<ScheduleConstraintType> WEAK_CONSTRAINTS = EnumSet.complementOf(EnumSet.of(SHOWING));
}
