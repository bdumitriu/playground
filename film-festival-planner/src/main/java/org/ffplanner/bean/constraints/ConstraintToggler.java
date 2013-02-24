package org.ffplanner.bean.constraints;

import org.ffplanner.entity.*;

import javax.persistence.EntityManager;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConstraintToggler {

    protected final EntityManager entityManager;

    protected final Showing showing;

    protected final UserSchedule userSchedule;

    protected ConstraintToggler(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        this.entityManager = entityManager;
        this.showing = showing;
        this.userSchedule = userSchedule;
    }

    public void toggle(ScheduleConstraintType constraintType) {
        final UserScheduleConstraint constraints = getConstraints();
        if (constraints == null) {
            createConstraint(showing, userSchedule, constraintType);
        } else {
            toggleConstraint(constraintType, constraints);
        }
    }

    protected abstract void toggleConstraint(ScheduleConstraintType constraintType, UserScheduleConstraint constraints);

    private UserScheduleConstraint getConstraints() {
        return entityManager.find(UserScheduleConstraint.class,
                new UserScheduleConstraintId(userSchedule.getId(), showing.getId()));
    }

    private void createConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        final UserScheduleConstraint userScheduleConstraint = new UserScheduleConstraint();
        userScheduleConstraint.setShowing(showing);
        userScheduleConstraint.setUserSchedule(userSchedule);
        userScheduleConstraint.setConstraintType(constraintType);
        entityManager.persist(userScheduleConstraint);
    }
}
