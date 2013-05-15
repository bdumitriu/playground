package org.ffplanner.controller.constraints;

import org.ffplanner.def.ConstraintDefinition;

import java.util.Objects;

/**
 * A constraint indicating that the user wants to watch a given movie, without caring when/where exactly.
 *
 * @author Bogdan Dumitriu
 */
public class QualifiedMovieConstraint
        extends QualifiedConstraint implements ConstraintDefinition.Movie, Comparable<QualifiedMovieConstraint> {

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

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final QualifiedMovieConstraint other = (QualifiedMovieConstraint) obj;
        return Objects.equals(this.movieId, other.movieId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(movieId);
    }

    @Override
    public int compareTo(QualifiedMovieConstraint o) {
        return Long.compare(this.movieId, o.movieId);
    }
}
