package org.ffplanner.controller.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;

/**
 * A constraint, either user-set or system-derived, on a {@link Showing}.
 *
 * @author Bogdan Dumitriu
 */
public abstract class QualifiedConstraint {

    protected Long showingId;

    protected Short priority;

    protected QualifiedConstraint(Long showingId, Short priority) {
        this.showingId = showingId;
        this.priority = priority;
    }

    public Long getShowingId() {
        return showingId;
    }

    public abstract ScheduleConstraintType getScheduleConstraintType();

    public Short getPriority() {
        return priority;
    }
}
