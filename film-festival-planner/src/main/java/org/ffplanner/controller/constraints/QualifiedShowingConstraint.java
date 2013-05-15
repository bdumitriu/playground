package org.ffplanner.controller.constraints;

import org.ffplanner.def.ConstraintDefinition;

import java.util.Objects;

/**
 * A constraint indicating that the user wants to watch a given movie exactly at a given time and venue.
 *
 * @author Bogdan Dumitriu
 */
public class QualifiedShowingConstraint
        extends QualifiedConstraint implements ConstraintDefinition.Showing, Comparable<QualifiedShowingConstraint> {

    public QualifiedShowingConstraint(Long showingId, Short priority) {
        super(showingId, priority);
    }

    @Override
    public ScheduleConstraintType getScheduleConstraintType() {
        return ScheduleConstraintType.SHOWING;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final QualifiedConstraint other = (QualifiedConstraint) obj;
        return Objects.equals(this.showingId, other.showingId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(showingId);
    }

    @Override
    public int compareTo(QualifiedShowingConstraint o) {
        return Long.compare(this.showingId, o.showingId);
    }
}
