package org.ffplanner.controller;

import com.google.common.base.Objects;
import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.def.ScheduleDefinition;
import org.ffplanner.entity.*;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsData {

    private final UserScheduleBean userScheduleBean;

    private final FestivalEdition festivalEdition;

    private final Map<Long, QualifiedConstraint> constraints;

    private UserSchedule userSchedule;

    private ConstraintsDefinition constraintsDefinition;

    public ConstraintsData(UserScheduleBean userScheduleBean, FestivalEdition festivalEdition) {
        this.userScheduleBean = userScheduleBean;
        this.festivalEdition = festivalEdition;
        this.constraints = new HashMap<>();
    }

    public int size() {
        return constraints.size();
    }

    public void loadFor(User user) {
        constraints.clear();
        userSchedule = userScheduleBean.findOrCreateBy(user.getId(), festivalEdition);
        for (UserScheduleConstraint userScheduleConstraint : userSchedule.getConstraints()) {
            final QualifiedConstraint qualifiedConstraint = new QualifiedConstraint(
                    userScheduleConstraint.getConstraintType(), userScheduleConstraint.getPriority());
            constraints.put(userScheduleConstraint.getShowing().getId(), qualifiedConstraint);
        }
        constraintsDefinition = null;
    }

    public boolean reloadNeeded() {
        final Date lastUpdate = userSchedule.getLastModified();
        final UserSchedule currentSchedule = userScheduleBean.find(userSchedule.getId());
        return scheduleDeleted(currentSchedule) || scheduleUpdatedSince(lastUpdate, currentSchedule);
    }

    private static boolean scheduleDeleted(UserSchedule currentSchedule) {
        return currentSchedule == null;
    }

    private static boolean scheduleUpdatedSince(Date lastUpdate, UserSchedule currentSchedule) {
        return currentSchedule != null && !Objects.equal(lastUpdate, currentSchedule.getLastModified());
    }

    /**
     * @return a {@link ScheduleDefinition} based on the current state of the constraints. This result is not updated
     *         automatically if the constraints change.
     */
    public ScheduleDefinition asScheduleDefinition() {
        if (constraintsDefinition == null) {
            constraintsDefinition = new ConstraintsDefinition();
            constraintsDefinition.initializeFrom(constraints);
        }
        return constraintsDefinition;
    }

    public boolean isConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        final QualifiedConstraint qualifiedConstraint = constraints.get(showingId);
        if (qualifiedConstraint != null) {
            final ScheduleConstraintType scheduleConstraintType = qualifiedConstraint.getScheduleConstraintType();
            return scheduleConstraintType == constraintType;
        } else {
            return false;
        }
    }

    /**
     * @return true if any constraint is set for {@code showingId}.
     */
    public boolean isAnyConstraintSelected(Long showingId) {
        final QualifiedConstraint qualifiedConstraint = constraints.get(showingId);
        return qualifiedConstraint != null;
    }

    public Short getConstraintPriority(Long showingId) {
        final QualifiedConstraint qualifiedConstraint = constraints.get(showingId);
        if (qualifiedConstraint != null) {
            return qualifiedConstraint.getPriority();
        } else {
            return null;
        }
    }
}
