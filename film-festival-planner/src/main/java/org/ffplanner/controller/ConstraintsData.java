package org.ffplanner.controller;

import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.entity.*;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsData {

    private final UserScheduleBean userScheduleBean;

    private final FestivalEdition festivalEdition;

    private Map<Long, ScheduleConstraintType> constraints;

    public ConstraintsData(UserScheduleBean userScheduleBean, FestivalEdition festivalEdition) {
        this.userScheduleBean = userScheduleBean;
        this.festivalEdition = festivalEdition;
    }

    public int size() {
        return constraints.size();
    }

    public void loadFor(User user) {
        constraints = new HashMap<>();
        final UserSchedule userSchedule = userScheduleBean.findOrCreateBy(user.getId(), festivalEdition);
        for (UserScheduleConstraint userScheduleConstraint : userSchedule.getConstraints()) {
            constraints.put(userScheduleConstraint.getShowing().getId(), userScheduleConstraint.getConstraintType());
        }
    }

    public boolean isConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        final ScheduleConstraintType scheduleConstraintType = constraints.get(showingId);
        return scheduleConstraintType != null && scheduleConstraintType == constraintType;
    }

    public boolean isDifferentConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        final ScheduleConstraintType scheduleConstraintType = constraints.get(showingId);
        return scheduleConstraintType != null && scheduleConstraintType != constraintType;
    }

    /**
     * @return true if any constraint is set for {@code showingId}.
     */
    public boolean isAnyConstraintSelected(Long showingId) {
        final ScheduleConstraintType scheduleConstraintType = constraints.get(showingId);
        return scheduleConstraintType != null;
    }
}
