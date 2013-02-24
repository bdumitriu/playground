package org.ffplanner.bean.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

/**
 * @author Bogdan Dumitriu
 */
public class AnyConstraintToggler extends ConstraintToggler {

    public AnyConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        super(entityManager, showing, userSchedule);
    }

    @Override
    protected void toggleConstraint(ScheduleConstraintType constraintType, UserScheduleConstraint constraints) {
        entityManager.remove(constraints);
    }
}
