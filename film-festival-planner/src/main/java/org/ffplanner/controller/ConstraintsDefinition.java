package org.ffplanner.controller;

import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.def.ScheduleDefinition;

import java.util.Collection;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsDefinition implements ScheduleDefinition {

    private Map<Long, QualifiedConstraint> constraints;

    public void initializeFrom(Map<Long, QualifiedConstraint> constraints) {
        this.constraints = constraints;
    }

    @Override
    public Collection<Long> getShowingIds() {
        return constraints.keySet();
    }

    @Override
    public ConstraintDefinition getConstraint(Long showingId) {
        return constraints.get(showingId);
    }
}
