/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.controller.auth.AuthData;
import org.ffplanner.entity.*;
import org.ffplanner.qualifier.Messages;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserBean extends BasicEntityBean<User> implements Serializable {

    private static final long serialVersionUID = -1563393265561085150L;

    @Inject @Messages
    private transient ResourceBundle bundle;

    @Override
    protected SingularAttribute<User, Long> getIdAttribute() {
        return User_.id;
    }

    @Override
    protected Class<User> getEntityClass() {
        return User.class;
    }

    public User getUserWithOpenId(String openId) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<UserToken> query = criteriaBuilder.createQuery(UserToken.class);
        final Root<UserToken> root = query.from(UserToken.class);
        query.where(criteriaBuilder.equal(root.get(UserToken_.token), openId));
        final TypedQuery<UserToken> userTokenQuery = entityManager.createQuery(query);
        final List<UserToken> result = userTokenQuery.getResultList();
        if (result.isEmpty()) {
            return null;
        } else {
            assert result.size() == 1;
            final User user = result.get(0).getUser();
            forceLazyLoad(user);
            return user;
        }
    }

    public User addUser(AuthData authData) {
        final User user = new User();
        user.setFirstName(authData.getFirstName());
        user.setLastName(authData.getLastName());
        user.setEmailAddress(authData.getEmailAddress());
        final UserToken userToken = new UserToken();
        userToken.setUser(user);
        userToken.setToken(authData.getOpenId());
        entityManager.persist(user);
        entityManager.persist(userToken);
        return user;
    }

    public UserSchedule getScheduleFor(Long userId, FestivalEdition festivalEdition) {
        return getScheduleFor(userId, festivalEdition, true);
    }

    public Long getScheduleIdFor(Long userId, FestivalEdition festivalEdition) {
        return getScheduleFor(userId, festivalEdition, false).getId();
    }

    public UserSchedule getScheduleFor(Long userId, FestivalEdition festivalEdition, boolean forceLazyLoad) {
        final User user = find(userId);
        final List<UserScheduleUseHistory> userSchedules = user.getSchedules();
        final UserSchedule userSchedule;
        if (userSchedules.isEmpty()) {
            userSchedule = new UserSchedule();
            userSchedule.setScheduleName(bundle.getString("MySchedule"));
            userSchedule.setFestivalEdition(festivalEdition);
            final UserScheduleUseHistory userScheduleUseHistory = new UserScheduleUseHistory();
            userScheduleUseHistory.setUser(user);
            userScheduleUseHistory.setUserSchedule(userSchedule);
            entityManager.persist(userSchedule);
            entityManager.persist(userScheduleUseHistory);
        } else {
            userSchedule = userSchedules.get(0).getUserSchedule();
        }
        if (forceLazyLoad) {
            forceLazyLoad(userSchedule);
        }
        return userSchedule;
    }

    private static void forceLazyLoad(User user) {
        user.getSchedules().iterator();
    }

    private static void forceLazyLoad(UserSchedule userSchedule) {
        userSchedule.getShowings().iterator();
        userSchedule.getConstraints().iterator();
        userSchedule.getUseHistory().iterator();
    }
}
