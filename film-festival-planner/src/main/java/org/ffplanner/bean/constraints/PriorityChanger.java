package org.ffplanner.bean.constraints;

import org.ffplanner.entity.MovieBundleConstraint;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.ShowingConstraint;
import org.ffplanner.entity.UserSchedule;

import javax.persistence.EntityManager;
import java.util.Collection;

/**
 * Change the priority associated to a movie or showing.
 *
 * @author Bogdan Dumitriu
 */
public class PriorityChanger extends ConstraintChanger {

    public PriorityChanger(EntityManager entityManager,
            Showing showing, Collection<Showing> otherShowings, UserSchedule userSchedule) {
        super(entityManager, showing, otherShowings, userSchedule);
    }

    public void change(Short priority) {
        final ShowingConstraint showingConstraint = getShowingConstraint();
        final MovieBundleConstraint movieConstraint = getMovieConstraint();
        if (showingConstraint != null && movieConstraint != null) {
            assert false;
        } else if (showingConstraint != null) {
            showingConstraint.setPriority(priority);
            entityManager.merge(showingConstraint);
        } else if (movieConstraint != null) {
            movieConstraint.setPriority(priority);
            entityManager.merge(movieConstraint);
        }
    }
}
