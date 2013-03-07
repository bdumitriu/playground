package org.ffplanner.bean.constraints;

import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraint;

import javax.persistence.EntityManager;

/**
 * Change the priority associated to a movie or showing.
 *
 * @author Bogdan Dumitriu
 */
public class PriorityChanger extends ConstraintChanger {

    public PriorityChanger(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        super(entityManager, showing, userSchedule);
    }

    public void change(Short priority) {
        final UserScheduleConstraint constraint = getConstraint();
        if (constraint != null) {
            constraint.setPriority(priority);
            entityManager.merge(constraint);
        }
    }
}
