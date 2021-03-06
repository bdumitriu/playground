package org.ffplanner.bean;

import org.ffplanner.bean.constraints.MovieConstraintToggler;
import org.ffplanner.bean.constraints.MovieConstraintViaShowingToggler;
import org.ffplanner.bean.constraints.PriorityChanger;
import org.ffplanner.bean.constraints.ShowingConstraintToggler;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleConstraintsBean implements Serializable {

    private static final long serialVersionUID = 1L;

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    private List<Showing> getShowingsForSameMovieAs(Showing showing) {
        final FestivalEditionProgramme programme = festivalProgrammeBean.getProgrammeFor(showing.getFestivalEdition());
        return programme.getShowingsForSameMovieAs(showing);
    }

    private List<Showing> getShowingsFor(MovieBundleInFestival movieBundle) {
        final FestivalEditionProgramme programme =
                festivalProgrammeBean.getProgrammeFor(movieBundle.getFestivalEditionSection().getFestivalEdition());
        return programme.getShowingsFor(movieBundle);
    }

    /**
     * Toggles the movie constraint for {@code movieBundle} as described in {@link MovieConstraintToggler}.
     */
    public void toggleMovieConstraint(MovieBundleInFestival movieBundle, UserSchedule userSchedule) {
        new MovieConstraintToggler(entityManager, movieBundle, getShowingsFor(movieBundle), userSchedule).change();
    }

    /**
     * Toggles the movie constraint for the movie of {@code showing} as described in
     * {@link MovieConstraintViaShowingToggler}.
     */
    public void toggleMovieConstraintViaShowing(Showing showing, UserSchedule userSchedule) {
        new MovieConstraintViaShowingToggler(
                entityManager, showing, getShowingsForSameMovieAs(showing), userSchedule).change();
    }

    /**
     * Toggles the showing constraint for {@code showing} as described in {@link ShowingConstraintToggler}.
     */
    public void toggleShowingConstraint(Showing showing, UserSchedule userSchedule) {
        new ShowingConstraintToggler(entityManager, showing, getShowingsForSameMovieAs(showing), userSchedule).change();
    }

    public void setConstraintPriority(Showing showing, UserSchedule userSchedule, Short priority) {
        new PriorityChanger(entityManager, showing, getShowingsForSameMovieAs(showing), userSchedule).change(priority);
    }

    public boolean hasMovieConstraint(Showing showing, UserSchedule userSchedule) {
        final MovieBundleConstraint movieConstraint = entityManager.find(MovieBundleConstraint.class,
                new MovieBundleConstraintId(userSchedule.getId(), showing.getMovieBundleInFestival().getId()));
        return movieConstraint != null;
    }

    public boolean hasShowingConstraint(Showing showing, UserSchedule userSchedule) {
        final ShowingConstraint showingConstraint = entityManager.find(
                ShowingConstraint.class, new ShowingConstraintId(userSchedule.getId(), showing.getId()));
        return showingConstraint != null;
    }
}
