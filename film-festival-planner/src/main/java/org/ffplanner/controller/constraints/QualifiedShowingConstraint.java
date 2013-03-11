package org.ffplanner.controller.constraints;

import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.entity.ScheduleConstraintType;

/**
 * A constraint indicating that the user wants to watch a given movie exactly at a given time and venue.
 *
 * @author Bogdan Dumitriu
 */
public class QualifiedShowingConstraint extends QualifiedConstraint implements ConstraintDefinition.Showing {

    public QualifiedShowingConstraint(Long showingId, Short priority) {
        super(showingId, priority);
        this.showingId = showingId;
    }

    @Override
    public ScheduleConstraintType getScheduleConstraintType() {
        return ScheduleConstraintType.SHOWING;
    }
}
