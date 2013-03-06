package org.ffplanner.bean;

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
import java.util.Date;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleBean extends BasicEntityBean<UserSchedule> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject @Messages
    private transient ResourceBundle bundle;

    @Inject
    private UserBean userBean;

    @Inject
    private ShowingBean showingBean;

    @Inject
    private UserScheduleConstraintsBean constraintsBean;

    @Override
    protected Class<UserSchedule> getEntityClass() {
        return UserSchedule.class;
    }

    @Override
    protected SingularAttribute<UserSchedule, Long> getIdAttribute() {
        return UserSchedule_.id;
    }

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
                userSchedule.loadLazyFields();
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

    /**
     * If {@code constraintType} is set, it is removed. If no constraint is set or any other constraint is set,
     * {@code constraintType} becomes the new constraint.
     */
    public void toggleConstraint(Long showingId, Long userId, final ScheduleConstraintType constraintType) {
        new ConstraintToggler() {
            @Override
            protected void toggle(Showing showing, UserSchedule userSchedule) {
                constraintsBean.toggleConstraint(showing, userSchedule, constraintType);
            }
        }.toggleConstraint(showingId, userId);
    }

    /**
     * If any constraint is set, it is removed. If no constraint is set, {@code constraintType} becomes the new
     * constraint.
     */
    public void toggleAnyConstraint(Long showingId, Long userId, final ScheduleConstraintType constraintType) {
        new ConstraintToggler() {
            @Override
            protected void toggle(Showing showing, UserSchedule userSchedule) {
                constraintsBean.toggleAnyConstraint(showing, userSchedule, constraintType);
            }
        }.toggleConstraint(showingId, userId);
    }

    /**
     * If {@code constraintType} is set, it replaced with {@code baseConstraintType}. If no constraint is set or any
     * other constraint is set, {@code constraintType} becomes the new constraint.
     */
    public void toggleFallbackConstraint(Long showingId, Long userId, final ScheduleConstraintType constraintType,
            final ScheduleConstraintType baseConstraintType) {
        new ConstraintToggler() {
            @Override
            protected void toggle(Showing showing, UserSchedule userSchedule) {
                constraintsBean.toggleFallbackConstraint(showing, userSchedule, constraintType, baseConstraintType);
            }
        }.toggleConstraint(showingId, userId);
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

    private abstract class ConstraintToggler {

        protected void toggleConstraint(Long showingId, Long userId) {
            final Showing showing = showingBean.find(showingId);
            if (showing != null) {
                final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
                toggle(showing, userSchedule);
                userSchedule.setLastModified(new Date());
                entityManager.merge(userSchedule);
            }
        }

        protected abstract void toggle(Showing showing, UserSchedule userSchedule);
    }
}
