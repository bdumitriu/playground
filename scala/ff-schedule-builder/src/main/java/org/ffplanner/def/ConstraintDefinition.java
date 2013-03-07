package org.ffplanner.def;

/**
 * @author Bogdan Dumitriu
 */
public interface ConstraintDefinition {

    enum WatchType {
        SHOWING,
        MOVIE
    }

    WatchType getWatchType();

    Short getPriority();
}
