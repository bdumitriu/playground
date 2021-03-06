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
import org.ffplanner.controller.constraints.ConstraintsIo;
import org.ffplanner.controller.constraints.ScheduleConstraintType;
import org.ffplanner.controller.schedule.ScheduleData;
import org.ffplanner.entity.FestivalEdition;
import org.ffplanner.entity.Showing;
import org.ffplanner.entity.User;
import org.ffplanner.qualifier.LoggedInUser;
import org.ffplanner.util.JsfViews;
import org.xml.sax.SAXException;

import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import javax.inject.Inject;
import javax.inject.Named;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;
import java.nio.file.*;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import static org.ffplanner.controller.constraints.ScheduleConstraintType.*;
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

    private FestivalEditionProgramme festivalProgramme;

    private Date festivalProgrammeLastInit;

    private Short priority;

    private ConstraintsData constraintsData;

    private ScheduleData scheduleData;

    private String constraintsFile = "foo";

    private FestivalEdition getFestivalEdition() {
        return festivalEditionBean.find(DEFAULT_FESTIVAL_EDITION_ID);
    }

    private FestivalEditionProgramme getFestivalEditionProgramme() {
        if (festivalProgramme == null || festivalProgrammeBean.changedAfter(festivalProgrammeLastInit)) {
            final FestivalEdition festivalEdition = getFestivalEdition();
            festivalProgramme = festivalProgrammeBean.getProgrammeFor(festivalEdition);
            festivalProgrammeLastInit = new Date();
        }
        return festivalProgramme;
    }

    public void updateConstraintsData() {
        if (constraintsData == null) {
            final FestivalEditionProgramme festivalProgramme = getFestivalEditionProgramme();
            constraintsData = new ConstraintsData(userScheduleBean, festivalProgramme);
            constraintsData.loadFor(this.user);
        } else if (constraintsData.reloadNeeded()) {
            constraintsData.loadFor(this.user);
        }
    }

    public void updateScheduleData() {
        if (scheduleData == null) {
            final FestivalEditionProgramme festivalProgramme = getFestivalEditionProgramme();
            scheduleData = new ScheduleData(userScheduleBean, festivalProgramme);
            scheduleData.loadFor(this.user);
        } else if (scheduleData.reloadNeeded()) {
            scheduleData.loadFor(this.user);
        }
    }

    public void removeInterestClickedViaShowing(Long showingId) {
        userScheduleBean.toggleMovieConstraintViaShowing(showingId, user.getId());
    }

    public void removeInterestClicked(Long movieBundleId) {
        userScheduleBean.toggleMovieConstraint(movieBundleId, user.getId());
    }

    public void watchMovieButtonClickedViaShowing(Long showingId) {
        userScheduleBean.toggleMovieConstraintViaShowing(showingId, user.getId());
    }

    public void watchMovieButtonClicked(Long movieBundleId) {
        userScheduleBean.toggleMovieConstraint(movieBundleId, user.getId());
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

    public void resetConstraints() {
        userScheduleBean.resetConstraints(user.getId(), getFestivalEdition());
    }

    public String suggestSchedule() {
        /*try {
            writeConstraints();
        } catch (IOException e) {
            e.printStackTrace();
        }*/
        final ScheduleBuilder scheduleBuilder = festivalProgrammeBean.getScheduleBuilder(DEFAULT_FESTIVAL_EDITION_ID);
        final List<Schedule> schedules = scheduleBuilder.getPossibleSchedulesJ(constraintsData.asScheduleConstraints());
        userScheduleBean.replaceSchedules(user.getId(), getFestivalEdition(), schedules);
        return JsfViews.MY_SCHEDULE_REDIRECT_TARGET.toString();
    }

    private void writeConstraints() throws IOException {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final String path = facesContext.getExternalContext().getRealPath("constraints.xml");
        final File file = new File(path);
        try (BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(file))) {
            final ConstraintsIo constraintsIo = new ConstraintsIo();
            constraintsIo.setMovieBundleInFestivalBean(movieBundleInFestivalBean);
            constraintsIo.setShowingBean(showingBean);
            constraintsIo.write(outputStream, constraintsData.asScheduleConstraints());
        }
    }

    public List<SelectItem> getConstraintFiles() {
        final Path scalaTestPath = getConstraintsPath();
        final List<SelectItem> constraintFiles = new LinkedList<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(scalaTestPath)) {
            for (Path file : stream) {
                if (Files.isRegularFile(file)) {
                    final String fileName = file.getFileName().toString();
                    if (fileName.startsWith("schedule_constraints_")) {
                        constraintFiles.add(new SelectItem(fileName));
                    }
                }
            }
        } catch (IOException | DirectoryIteratorException ignored) {
        }
        return constraintFiles;
    }

    public void setConstraintsFile(String constraintsFile) {
        this.constraintsFile = constraintsFile;
    }

    public String getConstraintsFile() {
        return constraintsFile;
    }

    public void loadConstraintsFile() {
        if (constraintsFile.startsWith("schedule_constraints_")) {
            final Path scalaTestPath = getConstraintsPath();
            try {
                loadConstraintsFile(scalaTestPath.resolve(constraintsFile).toFile());
            } catch (IOException | ParserConfigurationException | SAXException ignored) {
            }
        }
        constraintsFile = "foo";
    }

    private void loadConstraintsFile(File constraintsFile)
            throws IOException, ParserConfigurationException, SAXException {
        try (BufferedInputStream inputStream = new BufferedInputStream(new FileInputStream(constraintsFile))) {
            final ConstraintsIo constraintsIo = new ConstraintsIo();
            constraintsIo.setUserScheduleBean(userScheduleBean);
            constraintsIo.readToDatabase(inputStream, getFestivalEditionProgramme(), user.getId());
        }
    }

    private static Path getConstraintsPath() {
        final FacesContext facesContext = FacesContext.getCurrentInstance();
        final String scalaTestFileName =
                facesContext.getExternalContext().getRealPath("../../../scala/ff-schedule-builder/src/test/resources");
        return Paths.get(scalaTestFileName);
    }

    public String discardSchedule() {
        userScheduleBean.resetSchedules(user.getId(), getFestivalEdition());
        return JsfViews.PROGRAM_REDIRECT_TARGET.toString();
    }

    public boolean hasSchedule() {
        return scheduleData.size() > 0;
    }

    public boolean isScheduled(Long showingId) {
        return scheduleData.isScheduled(showingId);
    }

    public boolean couldNotBeScheduled(Long showingId) {
        return isWatchMovieSelected(showingId) && !isScheduled(showingId);
    }

    public boolean hasConstraints() {
        return constraintsData.size() > 0;
    }

    public boolean isScheduledElsewhere(Long showingId) {
        if (hasSchedule()) {
            for (Showing showing : getFestivalEditionProgramme().getShowingsForSameMovieAs(showingId)) {
                if (!showingId.equals(showing.getId()) && isScheduled(showing.getId())) {
                    return true;
                }
            }
        }
        return false;
    }

    public boolean isWatchMovieSelected(Long showingId) {
        return isConstraintSelected(showingId, MOVIE);
    }

    public boolean isWatchShowingSelected(Long showingId) {
        return isConstraintSelected(showingId, SHOWING);
    }

    public boolean isWatchShowingSelectedElsewhere(Long showingId) {
        return isConstraintSelected(showingId, SHOWING_ELSEWHERE);
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

    public boolean hasNoConstraintForShowing(Long showingId) {
        return constraintsData.hasNoConstraintForShowing(showingId);
    }

    public boolean hasNoConstraintForMovie(Long movieBundleId) {
        return constraintsData.hasNoConstraintForMovie(movieBundleId);
    }

    public short getShowingPriority(Long showingId) {
        final Short priorityObject = constraintsData.getConstraintPriority(showingId);
        return priorityObject == null ? -1 : priorityObject;
    }
}
