package org.ffplanner.def;

import java.util.Collection;

import static java.util.Collections.emptyList;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public interface ScheduleDefinition {

    ScheduleDefinition EMPTY = new Empty();

    class Empty implements ScheduleDefinition {

        private Empty() {}

        @Override
        public Collection<ConstraintDefinition.Showing> getShowingConstraints() {
            return emptyList();
        }

        @Override
        public Collection<ConstraintDefinition.Movie> getMovieConstraints() {
            return emptyList();
        }
    }

    Collection<ConstraintDefinition.Showing> getShowingConstraints();

    Collection<ConstraintDefinition.Movie> getMovieConstraints();
}
