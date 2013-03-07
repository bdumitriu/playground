package org.ffplanner.bean.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

/**
 * If the {@code constraintType} passed to {@link #change(ScheduleConstraintType)} is set, it is removed. If no
 * constraint is set or any other constraint is set, {@code constraintType} becomes the new constraint.
 *
 * @author Bogdan Dumitriu
 */
public class SpecificConstraintToggler extends ConstraintToggler {

    public SpecificConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        super(entityManager, showing, userSchedule);
    }

    @Override
    protected void toggleConstraint(ScheduleConstraintType constraintToRemoveOrSet, UserScheduleConstraint constraint) {
        if (constraint.getConstraintType() == constraintToRemoveOrSet) {
            entityManager.remove(constraint);
        } else {
            constraint.setConstraintType(constraintToRemoveOrSet);
        }
    }
}
