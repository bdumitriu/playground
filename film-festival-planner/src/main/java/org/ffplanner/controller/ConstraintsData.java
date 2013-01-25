package org.ffplanner.controller;

import org.ffplanner.bean.UserBean;
import org.ffplanner.entity.ScheduleConstraintType;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleConstraints;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsData {

    private final UserBean userBean;

    private Map<Long, ScheduleConstraintType> constraints;

    public ConstraintsData(UserBean userBean) {
        this.userBean = userBean;
    }

    public int size() {
        return constraints.size();
    }

    public void loadFor(User user) {
        constraints = new HashMap<>();
        final UserSchedule userSchedule = userBean.getScheduleFor(user.getId(), null);
        for (UserScheduleConstraints userScheduleConstraints : userSchedule.getConstraints()) {
            constraints.put(userScheduleConstraints.getShowing().getId(), userScheduleConstraints.getConstraintType());
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
