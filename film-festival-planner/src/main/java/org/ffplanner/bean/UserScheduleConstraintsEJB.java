/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleConstraintsEJB extends ConnectorEntityEJB<UserScheduleConstraints> {

    @Override
    protected Class<UserScheduleConstraints> getEntityClass() {
        return UserScheduleConstraints.class;
    }

    @Override
    protected Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<UserScheduleConstraints> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraints_.showingId).get(Showing_.id), id);
    }

    @Override
    protected Predicate getRightCondition(
            Long id, CriteriaBuilder criteriaBuilder, Root<UserScheduleConstraints> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraints_.userScheduleId).get(UserSchedule_.id), id);
    }

    @Override
    public UserScheduleConstraints find(Long showingId, Long userScheduleId) {
        return super.find(showingId, userScheduleId);
    }

    public void toggleConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        final UserScheduleConstraints constraints = entityManager.find(
                UserScheduleConstraints.class, new UserScheduleConstraintsId(userSchedule.getId(), showing.getId()));
        if (constraints == null) {
            final UserScheduleConstraints userScheduleConstraints = new UserScheduleConstraints();
            userScheduleConstraints.setShowing(showing);
            userScheduleConstraints.setUserSchedule(userSchedule);
            userScheduleConstraints.setConstraintType(constraintType);
            entityManager.persist(userScheduleConstraints);
        } else {
            if (constraints.getConstraintType() == constraintType) {
                entityManager.remove(constraints);
            } else {
                constraints.setConstraintType(constraintType);
            }
        }
    }

    public boolean hasConstraint(Showing showing, UserSchedule userSchedule, ScheduleConstraintType constraintType) {
        final UserScheduleConstraints constraints = entityManager.find(
                UserScheduleConstraints.class, new UserScheduleConstraintsId(userSchedule.getId(), showing.getId()));
        return constraints != null && constraints.getConstraintType() == constraintType;
    }
}
