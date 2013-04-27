package org.ffplanner.controller;

import org.ffplanner.Schedule;
import org.ffplanner.ScheduleBuilder;
import org.ffplanner.bean.FestivalEditionBean;
import org.ffplanner.bean.MovieBundleInFestivalBean;
import org.ffplanner.bean.ShowingBean;
import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.bean.programme.FestivalProgrammeBean;
import org.ffplanner.controller.constraints.ConstraintsData;
import org.ffplanner.controller.constraints.ConstraintsDumper;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.User;
import org.ffplanner.qualifier.LoggedInUser;

import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.inject.Named;
import java.io.*;
import java.util.LinkedList;
import java.util.List;

import static org.ffplanner.entity.ScheduleConstraintType.*;
import static org.ffplanner.util.ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID;

/**
 * @author Bogdan Dumitriu
 */
@Named
@SessionScoped
public class ScheduleController implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject @LoggedInUser
    private User user;

    @Inject
    private FestivalProgrammeBean festivalProgrammeBean;

    @Inject
    private ShowingBean showingBean;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    @Inject
    private MovieBundleInFestivalBean movieBundleInFestivalBean;

    @Inject
    private UserScheduleBean userScheduleBean;

    private Short priority;

    private ConstraintsData constraintsData;

    private List<Long> scheduledShowings;

    public void updateConstraintsData() {
        if (constraintsData == null) {
            final FestivalEdition festivalEdition = festivalEditionBean.find(DEFAULT_FESTIVAL_EDITION_ID);
            final FestivalEditionProgramme festivalProgramme = festivalProgrammeBean.getProgrammeFor(festivalEdition);
            constraintsData = new ConstraintsData(userScheduleBean, festivalProgramme);
            constraintsData.loadFor(this.user);
        } else if (constraintsData.reloadNeeded()) {
            constraintsData.loadFor(this.user);
        }
    }

    public void removeInterestClicked(Long showingId) {
        userScheduleBean.toggleMovieConstraint(showingId, user.getId());
        /* TODO: temporary */
        if (hasSchedule()) {
            scheduledShowings.remove(showingId);
        }
    }

    public void watchMovieButtonClicked(Long showingId) {
        userScheduleBean.toggleMovieConstraint(showingId, user.getId());
    }

    public void watchShowingButtonClicked(Long showingId) {
        userScheduleBean.toggleShowingConstraint(showingId, user.getId());
    }

    public void setPriority(Short priority) {
        this.priority = priority;
    }

    public Short getPriority() {
        return priority;
    }

    public void assignPriority(Long showingId) {
        userScheduleBean.setConstraintPriority(showingId, user.getId(), priority);
    }

    public boolean hasSchedule() {
        return scheduledShowings != null;
    }

    public void suggestSchedule() {
        try {
            dumpConstraints();
        } catch (IOException e) {
            e.printStackTrace();
        }
        final ScheduleBuilder scheduleBuilder = festivalProgrammeBean.getScheduleBuilder(DEFAULT_FESTIVAL_EDITION_ID);
        final Schedule schedule = scheduleBuilder.getPossibleSchedulesJ(constraintsData.asScheduleConstraints()).get(0);
        scheduledShowings = new LinkedList<>();
        scheduledShowings.addAll(schedule.showingIdsJ());
    }

    private void dumpConstraints() throws IOException{
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final String path = facesContext.getExternalContext().getRealPath("constraints.xml");
        final File file = new File(path);
        try (BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(file))) {
            final ConstraintsDumper constraintsDumper = new ConstraintsDumper(constraintsData.asScheduleConstraints());
            constraintsDumper.setMovieBundleInFestivalBean(movieBundleInFestivalBean);
            constraintsDumper.setShowingBean(showingBean);
            constraintsDumper.write(outputStream);
        }
    }

    public void discardSchedule() {
        scheduledShowings = null;
    }

    public boolean hasConstraints() {
        return constraintsData.size() > 0;
    }

    public boolean isScheduled(Long showingId) {
        return hasSchedule() && scheduledShowings.contains(showingId);
    }

    public boolean isWatchMovieSelected(Long showingId) {
        return isConstraintSelected(showingId, MOVIE);
    }

    public boolean isWatchShowingSelected(Long showingId) {
        return isConstraintSelected(showingId, SHOWING);
    }

    public boolean isWatchElsewhereSelected(Long showingId) {
        return isConstraintSelected(showingId, SHOWING_ELSEWHERE);
    }

    public String getMovieStyleClass(Long showingId) {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("sch_movie_cell");
        if (isWatchShowingSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_showing");
        } else if (isScheduled(showingId)) {
            stringBuilder.append(" sch_movie_cell_scheduled");
        } else if (isWatchMovieSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_movie");
        } else if (isWatchElsewhereSelected(showingId)) {
            stringBuilder.append(" sch_movie_cell_watch_elsewhere");
        }
        return stringBuilder.toString();
    }

    private boolean isConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        return constraintsData.isConstraintSelected(showingId, constraintType);
    }

    public boolean hasWeakConstraintFor(Long showingId) {
        return constraintsData.hasWeakConstraintFor(showingId);
    }

    public boolean hasUserConstraintFor(Long showingId) {
        return constraintsData.hasUserConstraintFor(showingId);
    }

    public boolean isNoConstraintSelectedFor(Long showingId) {
        return constraintsData.hasNoConstraintFor(showingId);
    }

    public short getShowingPriority(Long showingId) {
        final Short priorityObject = constraintsData.getConstraintPriority(showingId);
        return priorityObject == null ? -1 : priorityObject;
    }
}
