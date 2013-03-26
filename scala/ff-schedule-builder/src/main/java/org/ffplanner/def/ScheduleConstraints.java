package org.ffplanner.def;

import com.google.common.collect.ImmutableRangeSet;
import com.google.common.collect.Range;
import org.joda.time.DateTime;

import java.util.Collection;

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
        public ImmutableRangeSet<DateTime> getTimeConstraints() {
            return ImmutableRangeSet.of(Range.closedOpen(new DateTime(Long.MIN_VALUE), new DateTime(Long.MAX_VALUE)));
        }
    }

    Collection<ConstraintDefinition.Showing> getShowingConstraints();

    Collection<ConstraintDefinition.Movie> getMovieConstraints();

    /**
     * @return the time interval(s) when shows can be scheduled.
     */
    ImmutableRangeSet<DateTime> getTimeConstraints();
}
