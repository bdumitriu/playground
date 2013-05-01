package org.ffplanner.bean;

import org.ffplanner.bean.constraints.MovieConstraintToggler;
import org.ffplanner.bean.constraints.ShowingConstraintToggler;
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

    public void reset(Long userId, FestivalEdition festivalEdition) {
        reset(findOrCreateBy(userId, festivalEdition, false));
    }

    public void reset(UserSchedule userSchedule) {
        for (MovieBundleConstraint movieConstraint : userSchedule.getMovieConstraints()) {
            entityManager.remove(movieConstraint);
        }
        for (ShowingConstraint showingConstraint : userSchedule.getShowingConstraints()) {
            entityManager.remove(showingConstraint);
        }
        userSchedule.resetConstraints();
        userSchedule.setLastModified(new Date());
        entityManager.persist(userSchedule);
    }

    /**
     * Toggles the movie constraint for the movie of {@code showing} as described in {@link MovieConstraintToggler}.
     */
    public void toggleMovieConstraint(Long showingId, Long userId) {
        new ConstraintChanger() {
            @Override
            protected void change(Showing showing, UserSchedule userSchedule) {
                constraintsBean.toggleMovieConstraint(showing, userSchedule);
            }
        }.changeConstraint(showingId, userId);
    }

    /**
     * Toggles the showing constraint for {@code showing} as described in {@link ShowingConstraintToggler}.
     */
    public void toggleShowingConstraint(Long showingId, Long userId) {
        new ConstraintChanger() {
            @Override
            protected void change(Showing showing, UserSchedule userSchedule) {
                constraintsBean.toggleShowingConstraint(showing, userSchedule);
            }
        }.changeConstraint(showingId, userId);
    }

    public boolean isMovieConstraintSelected(Long showingId, Long userId) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
            return constraintsBean.hasMovieConstraint(showing, userSchedule);
        } else {
            return false;
        }
    }

    public boolean isShowingConstraintSelected(Long showingId, Long userId) {
        final Showing showing = showingBean.find(showingId);
        if (showing != null) {
            final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
            return constraintsBean.hasShowingConstraint(showing, userSchedule);
        } else {
            return false;
        }
    }

    public void setConstraintPriority(Long showingId, Long userId, final Short priority) {
        new ConstraintChanger() {
            @Override
            protected void change(Showing showing, UserSchedule userSchedule) {
                constraintsBean.setConstraintPriority(showing, userSchedule, priority);
            }
        }.changeConstraint(showingId, userId);
    }

    private abstract class ConstraintChanger {

        protected void changeConstraint(Long showingId, Long userId) {
            final Showing showing = showingBean.find(showingId);
            if (showing != null) {
                final UserSchedule userSchedule = findOrCreateBy(userId, showing.getFestivalEdition(), false);
                change(showing, userSchedule);
                userSchedule.setLastModified(new Date());
                entityManager.merge(userSchedule);
            }
        }

        protected abstract void change(Showing showing, UserSchedule userSchedule);
    }
}
