package org.ffplanner.def;

import java.util.Collection;

/**
 *
 *
 * @author Bogdan Dumitriu
 */
public interface ScheduleDefinition {

    Collection<Long> getShowingIds();

    ConstraintDefinition getConstraint(Long showingId);
}
