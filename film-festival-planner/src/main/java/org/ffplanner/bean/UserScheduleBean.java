/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.*;
import org.ffplanner.qualifier.Messages;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.io.Serializable;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleBean implements Serializable {

    private static final long serialVersionUID = 4498112316395205031L;

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @Inject @Messages
    private transient ResourceBundle bundle;

    @Inject
    private UserBean userBean;

    @Inject
    private ShowingBean showingBean;

    @Inject
    private UserScheduleConstraintsBean constraintsBean;

    public UserSchedule findOrCreateBy(Long userId, FestivalEdition festivalEdition) {
        return findOrCreateBy(userId, festivalEdition, true);
    }

    public Long findOrCreateIdBy(Long userId, FestivalEdition festivalEdition) {
        return findOrCreateBy(userId, festivalEdition, false).getId();
    }

    public UserSchedule findOrCreateBy(Long userId, FestivalEdition festivalEdition, boolean forceLazyLoad) {
        return findOrCreateBy(userBean.getReference(userId), festivalEdition, forceLazyLoad);
    }

    public UserSchedule findOrCreateBy(User user, FestivalEdition festivalEdition, boolean forceLazyLoad) {
        final List<UserSchedule> userSchedules = getUserSchedules(user, festivalEdition);
        if (userSchedules.isEmpty()) {
            return createWith(user, festivalEdition);
        } else {
            final UserSchedule userSchedule = userSchedules.get(0);
            if (forceLazyLoad) {
                forceLazyLoad(userSchedule);
            }
            return userSchedule;
        }
    }

    private List<UserSchedule> getUserSchedules(User user, FestivalEdition festivalEdition) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<UserSchedule> query = criteriaBuilder.createQuery(UserSchedule.class);
        final Root<UserSchedule> root = query.from(UserSchedule.class);
        query.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(UserSchedule_.user), user),
                criteriaBuilder.equal(root.get(UserSchedule_.festivalEdition), festivalEdition)
        ));
        query.orderBy(criteriaBuilder.desc(root.get(UserSchedule_.lastUsed)));
        final TypedQuery<UserSchedule> typedQuery = entityManager.createQuery(query);
        return typedQuery.getResultList();
    }

    public UserSchedule createWith(User user, FestivalEdition festivalEdition) {
        final UserSchedule userSchedule = new UserSchedule();
        userSchedule.setScheduleName(bundle.getString("MySchedule"));
        userSchedule.setUser(user);
        userSchedule.setFestivalEdition(festivalEdition);
        entityManager.persist(userSchedule);
        return userSchedule;
    }

    public void toggleConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
            constraintsBean.toggleConstraint(showing, userSchedule, constraintType);
        }
    }

    public void toggleAnyConstraint(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
            constraintsBean.toggleAnyConstraint(showing, userSchedule, constraintType);
        }
    }

    public boolean isConstraintSelected(Long showingId, Long userId, ScheduleConstraintType constraintType) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
            return constraintsBean.hasConstraint(showing, userSchedule, constraintType);
        } else {
            return false;
        }
    }

    private static void forceLazyLoad(UserSchedule userSchedule) {
        userSchedule.getShowings().iterator();
        userSchedule.getConstraints().iterator();
    }
}