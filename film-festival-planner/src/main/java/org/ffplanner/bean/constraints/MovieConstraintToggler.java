package org.ffplanner.bean.constraints;

import org.ffplanner.entity.*;

import javax.persistence.EntityManager;
import java.util.Collection;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_PRIORITY;

/**
 * Toggles a "watch no matter which showing of this movie" constraint as follows:
 * <ul>
 *     <li><i>if missing</i>, it creates it;</li>
 *     <li><i>if existing</i>, it removes it as well as the "watch this showing of a movie" constraint for the given
 *     {@code showing} (but other "watch this showing of a movie" for the same movie are maintained).</li>
 * </ul>
 *
 * @author Bogdan Dumitriu
 */
public class MovieConstraintToggler extends ConstraintToggler {

    public MovieConstraintToggler(EntityManager entityManager,
            Showing showing, Collection<Showing> otherShowings, UserSchedule userSchedule) {
        super(entityManager, showing, otherShowings, userSchedule);
    }

    @Override
    public void change() {
        final ShowingConstraint showingConstraint = getShowingConstraint();
        final MovieBundleConstraint movieConstraint = getMovieConstraint();
        if (movieConstraint == null) {
            onConstraintNotExists(showingConstraint);
        } else {
            onConstraintExists(showingConstraint, movieConstraint);
        }
    }

    private void onConstraintNotExists(ShowingConstraint showingConstraint) {
        if (showingConstraint == null) {
            if (otherShowingConstraints.isEmpty()) {
                createMovieConstraint(DEFAULT_PRIORITY);
            } else {
                assert false;
            }
        } else {
            if (otherShowingConstraints.isEmpty()) {
                removeShowingConstraint(showingConstraint);
            } else {
                new ShowingConstraintToggler(this).change();
            }
        }
    }

    private void onConstraintExists(ShowingConstraint showingConstraint, MovieBundleConstraint movieConstraint) {
        removeMovieConstraint(movieConstraint);
        if (showingConstraint != null) {
            removeShowingConstraint(showingConstraint);
        }
    }
}
