package org.ffplanner.def;

import org.joda.time.DateTime;
import org.joda.time.Interval;

import java.util.Collection;
import java.util.Collections;

import static java.util.Collections.emptyList;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public interface ScheduleConstraints {

    ScheduleConstraints EMPTY = new Empty();

    class Empty implements ScheduleConstraints {

        private Empty() {}

        @Override
        public Collection<ConstraintDefinition.Showing> getShowingConstraints() {
            return emptyList();
        }

        @Override
        public Collection<ConstraintDefinition.Movie> getMovieConstraints() {
            return emptyList();
        }

        @Override
        public Collection<Interval> getTimeConstraints() {
            return Collections.singleton(new Interval(new DateTime(Long.MIN_VALUE), new DateTime(Long.MAX_VALUE)));
        }
    }

    Collection<ConstraintDefinition.Showing> getShowingConstraints();

    Collection<ConstraintDefinition.Movie> getMovieConstraints();

    /**
     * @return the time interval(s) when shows can be scheduled.
     */
    Collection<Interval> getTimeConstraints();
}
