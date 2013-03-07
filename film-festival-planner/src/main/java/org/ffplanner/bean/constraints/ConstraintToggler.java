package org.ffplanner.bean.constraints;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_PRIORITY;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConstraintToggler extends ConstraintChanger {

    protected ConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        super(entityManager, showing, userSchedule);
    }

    public void change(ScheduleConstraintType constraintType) {
        final UserScheduleConstraint constraint = getConstraint();
        if (constraint == null) {
            createConstraint(showing, userSchedule, constraintType, DEFAULT_PRIORITY);
        } else {
            toggleConstraint(constraintType, constraint);
        }
    }

    protected abstract void toggleConstraint(ScheduleConstraintType constraintType, UserScheduleConstraint constraint);
}
