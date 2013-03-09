package org.ffplanner.bean.constraints;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import org.ffplanner.entity.*;

import javax.persistence.EntityManager;
import java.util.Collection;

/**
 * @author Bogdan Dumitriu
 */
public abstract class ConstraintChanger {

    protected final EntityManager entityManager;

    protected final Showing showing;

    protected final Collection<ShowingConstraint> otherShowingConstraints;

    protected final UserSchedule userSchedule;

    protected ConstraintChanger(EntityManager entityManager,
            final Showing showing, Collection<Showing> otherShowings, UserSchedule userSchedule) {
        this.entityManager = entityManager;
        this.showing = showing;
        this.otherShowingConstraints = Collections2.filter(
                Collections2.transform(otherShowings, new Function<Showing, ShowingConstraint>() {
                    @Override
                    public ShowingConstraint apply(Showing input) {
                        return getShowingConstraint(input.getId());
                    }
                }),
                new Predicate<ShowingConstraint>() {
                    @Override
                    public boolean apply(ShowingConstraint input) {
                        return input != null && (showing == null || showing.getId() != input.getShowing().getId());
                    }
                }
        );
        this.userSchedule = userSchedule;
    }

    protected ConstraintChanger(ConstraintChanger constraintChanger) {
        this.entityManager = constraintChanger.entityManager;
        this.showing = constraintChanger.showing;
        this.otherShowingConstraints = constraintChanger.otherShowingConstraints;
        this.userSchedule = constraintChanger.userSchedule;
    }

    protected ShowingConstraint getShowingConstraint() {
        final Long showingId = showing.getId();
        return getShowingConstraint(showingId);
    }

    private ShowingConstraint getShowingConstraint(Long showingId) {
        final ShowingConstraintId constraintId = new ShowingConstraintId(userSchedule.getId(), showingId);
        return entityManager.find(ShowingConstraint.class, constraintId);
    }

    protected MovieBundleConstraint getMovieConstraint() {
        final MovieBundleConstraintId constraintId =
                new MovieBundleConstraintId(userSchedule.getId(), getMovieBundle().getId());
        return entityManager.find(MovieBundleConstraint.class, constraintId);
    }

    protected void createShowingConstraint(Short priority) {
        final ShowingConstraint showingConstraint = new ShowingConstraint();
        showingConstraint.setShowing(showing);
        showingConstraint.setUserSchedule(userSchedule);
        showingConstraint.setPriority(priority);
        entityManager.persist(showingConstraint);
    }

    protected void createMovieConstraint(Short priority) {
        final MovieBundleConstraint movieBundleConstraint = new MovieBundleConstraint();
        movieBundleConstraint.setMovieBundle(getMovieBundle());
        movieBundleConstraint.setUserSchedule(userSchedule);
        movieBundleConstraint.setPriority(priority);
        entityManager.persist(movieBundleConstraint);
    }

    protected MovieBundleInFestival getMovieBundle() {
        return showing.getMovieBundleInFestival();
    }

    protected void createOrUpdateMovieConstraint(MovieBundleConstraint movieConstraint, Short priority) {
        if (movieConstraint == null) {
            createMovieConstraint(priority);
        } else {
            movieConstraint.setPriority(priority);
            entityManager.merge(movieConstraint);
        }
    }

    protected void removeShowingConstraint(ShowingConstraint showingConstraint) {
        if (showingConstraint != null) {
            entityManager.remove(showingConstraint);
        }
    }

    protected void removeMovieConstraint(MovieBundleConstraint movieConstraint) {
        if (movieConstraint != null) {
            entityManager.remove(movieConstraint);
        }
    }
}
