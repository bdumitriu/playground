/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ScheduleBean implements Serializable {

    private static final long serialVersionUID = 4498112316395205031L;

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @Inject
    private UserBean userBean;

    @Inject
    private ShowingBean showingBean;

    @Inject
    private UserScheduleConstraintsBean constraintsBean;

    public void toggleConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userBean.getScheduleFor(userId, showing.getFestivalEdition(), false);
            constraintsBean.toggleConstraint(showing, userSchedule, constraintType);
        }
    }

    public void toggleAnyConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userBean.getScheduleFor(userId, showing.getFestivalEdition(), false);
            constraintsBean.toggleAnyConstraint(showing, userSchedule, constraintType);
        }
    }

    public boolean isConstraintSelected(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userBean.getScheduleFor(userId, showing.getFestivalEdition(), false);
            return constraintsBean.hasConstraint(showing, userSchedule, constraintType);
        } else {
            return false;
        }
    }
}
