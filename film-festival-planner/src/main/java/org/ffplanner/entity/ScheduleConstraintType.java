package org.ffplanner.entity;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public enum ScheduleConstraintType implements Serializable {

    // want to watch exactly this showing
    SHOWING,

    // want to watch the movie, on this or some other showing
    MOVIE,

    // watch this movie, if possible
    MAYBE_MOVIE
}
