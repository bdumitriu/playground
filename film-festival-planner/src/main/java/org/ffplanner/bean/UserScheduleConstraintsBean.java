package org.ffplanner.bean;

import org.ffplanner.bean.constraints.ShowingConstraintToggler;
import org.ffplanner.bean.constraints.MovieConstraintToggler;
import org.ffplanner.bean.constraints.PriorityChanger;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class UserScheduleConstraintsBean extends ConnectorEntityBean<ShowingConstraint> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    @Override
    public ShowingConstraint find(Long showingId, Long userScheduleId) {
        return super.find(showingId, userScheduleId);
    }

    @Override
    protected Class<ShowingConstraint> getEntityClass() {
        return ShowingConstraint.class;
    }

    @Override
    protected Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<ShowingConstraint> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraint_.showing).get(Showing_.id), id);
    }

    @Override
    protected Predicate getRightCondition(
            Long id, CriteriaBuilder criteriaBuilder, Root<ShowingConstraint> root) {
        return criteriaBuilder.equal(root.get(UserScheduleConstraint_.userSchedule).get(UserSchedule_.id), id);
    }

    private List<Showing> getShowingsForSameMovieAs(Showing showing) {
        final FestivalEditionProgramme programme = festivalProgrammeBean.getProgrammeFor(showing.getFestivalEdition());
        return programme.getShowingsForSameMovieAs(showing);
    }

    /**
     * Toggles the movie constraint for the movie of {@code showing} as described in {@link MovieConstraintToggler}.
     */
    public void toggleMovieConstraint(Showing showing, UserSchedule userSchedule) {
        new MovieConstraintToggler(entityManager, showing, getShowingsForSameMovieAs(showing), userSchedule).change();
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
