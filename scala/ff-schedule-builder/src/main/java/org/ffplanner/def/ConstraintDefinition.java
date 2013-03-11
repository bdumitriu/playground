package org.ffplanner.def;

/**
 * @author Bogdan Dumitriu
 */
public interface ConstraintDefinition {

    Short getPriority();

    public interface Movie extends ConstraintDefinition {
        Long getMovieId();
    }

    public interface Showing extends ConstraintDefinition {
        Long getShowingId();
    }
}
