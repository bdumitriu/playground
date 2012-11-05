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
        final UserScheduleConstraints constraints = getConstraints();
        if (constraints == null) {
            createConstraint(showing, userSchedule, constraintType);
        } else {
            foo(constraintType, constraints);
        }
    }

    protected abstract void foo(ScheduleConstraintType constraintType, UserScheduleConstraints constraints);

    private UserScheduleConstraints getConstraints() {
        return entityManager.find(UserScheduleConstraints.class,
                new UserScheduleConstraintsId(userSchedule.getId(), showing.getId()));
    }

    private void createConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        final UserScheduleConstraints userScheduleConstraints = new UserScheduleConstraints();
        userScheduleConstraints.setShowing(showing);
        userScheduleConstraints.setUserSchedule(userSchedule);
        userScheduleConstraints.setConstraintType(constraintType);
        entityManager.persist(userScheduleConstraints);
    }
}
