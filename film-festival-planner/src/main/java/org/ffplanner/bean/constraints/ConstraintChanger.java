package org.ffplanner.bean.constraints;

import org.ffplanner.entity.*;

import javax.persistence.EntityManager;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConstraintChanger {

    protected final EntityManager entityManager;

    protected final Showing showing;

    protected final UserSchedule userSchedule;

    protected ConstraintChanger(EntityManager entityManager, Showing showing, UserSchedule userSchedule) {
        this.entityManager = entityManager;
        this.showing = showing;
        this.userSchedule = userSchedule;
    }

    protected UserScheduleConstraint getConstraint() {
        return entityManager.find(
                UserScheduleConstraint.class, new UserScheduleConstraintId(userSchedule.getId(), showing.getId()));
    }

    protected void createConstraint(
            Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType, Short defaultPriority) {
        final UserScheduleConstraint userScheduleConstraint = new UserScheduleConstraint();
        userScheduleConstraint.setShowing(showing);
        userScheduleConstraint.setUserSchedule(userSchedule);
        userScheduleConstraint.setConstraintType(constraintType);
        userScheduleConstraint.setPriority(defaultPriority);
        entityManager.persist(userScheduleConstraint);
    }
}
