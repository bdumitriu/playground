package org.ffplanner.def;

import java.util.List;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public interface ScheduleDefinition {

    enum WatchType {
        SHOWING,
        MOVIE,
        MAYBE_MOVIE
    }

    List<Long> getShowingIds(WatchType watchType);
}
