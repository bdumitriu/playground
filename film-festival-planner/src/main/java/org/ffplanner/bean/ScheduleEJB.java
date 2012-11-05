/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.UserSchedule;

import javax.ejb.EJB;
import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ScheduleEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @EJB
    private UserEJB userEJB;

    @EJB
    private ShowingEJB showingEJB;

    @EJB
    private UserScheduleConstraintsEJB constraintsEJB;

    public void toggleConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingEJB.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userEJB.getScheduleFor(userId, showing.getFestivalEdition(), false);
            constraintsEJB.toggleConstraint(showing, userSchedule, constraintType);
        }
    }

    public void toggleAnyConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingEJB.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userEJB.getScheduleFor(userId, showing.getFestivalEdition(), false);
            constraintsEJB.toggleAnyConstraint(showing, userSchedule, constraintType);
        }
    }

    public boolean isConstraintSelected(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingEJB.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = userEJB.getScheduleFor(userId, showing.getFestivalEdition(), false);
            return constraintsEJB.hasConstraint(showing, userSchedule, constraintType);
        } else {
            return false;
        }
    }
}
