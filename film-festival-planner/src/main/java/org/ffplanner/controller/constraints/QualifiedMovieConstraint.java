package org.ffplanner.controller.constraints;

import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.entity.ScheduleConstraintType;

/**
 * A constraint indicating that the user wants to watch a given movie, without caring when/where exactly.
 *
 * @author Bogdan Dumitriu
 */
public class QualifiedMovieConstraint extends QualifiedConstraint implements ConstraintDefinition.Movie {

    private final Long movieId;

    public QualifiedMovieConstraint(Long showingId, Long movieId, Short priority) {
        super(showingId, priority);
        this.movieId = movieId;
    }

    @Override
    public ScheduleConstraintType getScheduleConstraintType() {
        return ScheduleConstraintType.MOVIE;
    }

    @Override
    public Long getMovieId() {
        return movieId;
    }
}
