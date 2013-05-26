package org.ffplanner.bean.constraints;

import org.ffplanner.entity.*;

import javax.persistence.EntityManager;
import java.util.Collection;
import java.util.LinkedList;

import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_PRIORITY;

/**
 * Toggles a "watch no matter which showing of this movie" constraint as follows:
 * <ul>
 *     <li><i>if missing</i>, it creates it;</li>
 *     <li><i>if existing</i>, it removes it as well as any "watch this showing of a movie" constraint for all showings
 *     of this movie.</li>
 * </ul>
 *
 * @author Bogdan Dumitriu
 */
public class MovieConstraintToggler extends ConstraintToggler {

    private final MovieBundleInFestival movieBundle;

    public MovieConstraintToggler(EntityManager entityManager, MovieBundleInFestival movieBundle,
            Collection<Showing> allShowings, UserSchedule userSchedule) {
        super(entityManager, null, allShowings, userSchedule);
        this.movieBundle = movieBundle;
    }

    @Override
    protected MovieBundleInFestival getMovieBundle() {
        return movieBundle;
    }

    @Override
    public void change() {
        final Collection<ShowingConstraint> showingConstraints = getShowingConstraints();
        final MovieBundleConstraint movieConstraint = getMovieConstraint();
        if (movieConstraint == null) {
            onConstraintNotExists(showingConstraints);
        } else {
            onConstraintExists(showingConstraints, movieConstraint);
        }
    }

    private Collection<ShowingConstraint> getShowingConstraints() {
        final Collection<ShowingConstraint> showingConstraints = new LinkedList<>();
        // since null is passed to the parent class, otherShowingConstraint will contain all showing constraints
        for (ShowingConstraint otherShowingConstraint : otherShowingConstraints) {
            final ShowingConstraint showingConstraint =
                    getShowingConstraint(otherShowingConstraint.getShowing().getId());
            if (showingConstraint != null) {
                showingConstraints.add(showingConstraint);
            }
        }
        return showingConstraints;
    }

    private void onConstraintNotExists(Collection<ShowingConstraint> showingConstraints) {
        if (showingConstraints.isEmpty()) {
            createMovieConstraint(DEFAULT_PRIORITY);
        } else {
            removeAll(showingConstraints);
        }
    }

    private void onConstraintExists(
            Iterable<ShowingConstraint> showingConstraints, MovieBundleConstraint movieConstraint) {
        removeMovieConstraint(movieConstraint);
        removeAll(showingConstraints);
    }

    private void removeAll(Iterable<ShowingConstraint> showingConstraints) {
        for (ShowingConstraint showingConstraint : showingConstraints) {
            removeShowingConstraint(showingConstraint);
        }
    }
}
