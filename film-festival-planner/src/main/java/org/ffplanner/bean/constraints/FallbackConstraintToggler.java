package org.ffplanner.bean.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

/**
 * If the {@code constraintType} passed to {@link #toggle(ScheduleConstraintType)} is set, it replaced with a
 * preconfigured, different {@code baseConstraintType}. If no constraint is set or any other constraint is set,
 * {@code constraintType} becomes the new constraint.
 *
 * @author Bogdan Dumitriu
 */
public class FallbackConstraintToggler extends ConstraintToggler {

    private final ScheduleConstraintType baseConstraintType;

    public FallbackConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule,
            ScheduleConstraintType baseConstraintType) {
        super(entityManager, showing, userSchedule);
        this.baseConstraintType = baseConstraintType;
    }

    @Override
    protected void toggleConstraint(
            ScheduleConstraintType constraintToRemoveOrSet, UserScheduleConstraint constraints) {
        if (constraints.getConstraintType() == constraintToRemoveOrSet) {
            constraints.setConstraintType(baseConstraintType);
        } else {
            constraints.setConstraintType(constraintToRemoveOrSet);
        }
    }
}
