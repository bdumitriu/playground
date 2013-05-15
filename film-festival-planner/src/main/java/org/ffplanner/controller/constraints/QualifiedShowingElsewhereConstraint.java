package org.ffplanner.controller.constraints;

/**
 * A pseudo-constraint indicating that the user selected a certain movie at a different time and venue.
 *
 * @author Bogdan Dumitriu
 */
public class QualifiedShowingElsewhereConstraint extends QualifiedConstraint {

    public QualifiedShowingElsewhereConstraint(Long showingId) {
        super(showingId, null);
    }

    @Override
    public ScheduleConstraintType getScheduleConstraintType() {
        return ScheduleConstraintType.SHOWING_ELSEWHERE;
    }
}
