package org.ffplanner.bean.constraints;

import org.ffplanner.entity.*;

import javax.persistence.EntityManager;

import java.util.Collection;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_PRIORITY;

/**
 * Toggles a "watch this showing of a movie" constraint as follows:
 * <ul>
 *     <li><i>if missing</i>, it creates it and also removes a possible "watch no matter which showing of this movie"
 *     constraint, taking over its priority;</li>
 *     <li><i>if existing</i>, it removes it and creates a "watch no matter which showing of this movie" constraint with
 *     the same priority instead.</li>
 * </ul>
 *
 * @author Bogdan Dumitriu
 */
public class ShowingConstraintToggler extends ConstraintToggler {

    public ShowingConstraintToggler(EntityManager entityManager,
            Showing showing, Collection<Showing> otherShowings, UserSchedule userSchedule) {
        super(entityManager, showing, otherShowings, userSchedule);
    }

    protected ShowingConstraintToggler(ConstraintChanger constraintChanger) {
        super(constraintChanger);
    }

    @Override
    public void change() {
        final ShowingConstraint showingConstraint = getShowingConstraint();
        final MovieBundleConstraint movieConstraint = getMovieConstraint();
        if (showingConstraint == null) {
            onConstraintNotExists(movieConstraint);
        } else {
            onConstraintExists(showingConstraint, movieConstraint);
        }
    }

    private void onConstraintNotExists(MovieBundleConstraint movieConstraint) {
        final Short priority;
        if (movieConstraint == null) {
            priority = DEFAULT_PRIORITY;
        } else {
            priority = movieConstraint.getPriority();
            removeMovieConstraint(movieConstraint);
        }
        createShowingConstraint(priority);
    }

    private void onConstraintExists(ShowingConstraint showingConstraint, MovieBundleConstraint movieConstraint) {
        doSanityCheck(movieConstraint);
        removeShowingConstraint(showingConstraint);
        if (otherShowingConstraints.isEmpty()) {
            createOrUpdateMovieConstraint(movieConstraint, showingConstraint.getPriority());
        } else {
            removeMovieConstraint(movieConstraint);
        }
    }

    private static void doSanityCheck(MovieBundleConstraint movieConstraint) {
        if (movieConstraint != null) {
            assert false;
        }
    }
}
