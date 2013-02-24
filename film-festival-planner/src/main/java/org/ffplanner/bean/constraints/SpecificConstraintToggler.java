package org.ffplanner.bean.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

/**
 * @author Bogdan Dumitriu
 */
public class SpecificConstraintToggler extends ConstraintToggler {

    public SpecificConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        super(entityManager, showing, userSchedule);
    }

    @Override
    protected void toggleConstraint(ScheduleConstraintType constraintToRemoveOrSet, UserScheduleConstraint constraints) {
        if (constraints.getConstraintType() == constraintToRemoveOrSet) {
            entityManager.remove(constraints);
        } else {
            constraints.setConstraintType(constraintToRemoveOrSet);
        }
    }
}
