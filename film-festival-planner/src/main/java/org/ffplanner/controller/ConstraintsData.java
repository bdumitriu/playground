package org.ffplanner.controller;

import org.ffplanner.bean.UserEJB;
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

    private final UserEJB userEJB;

    private Map<Long, ScheduleConstraintType> constraints;

    public ConstraintsData(UserEJB userEJB) {
        this.userEJB = userEJB;
    }

    public void loadFor(User user) {
        constraints = new HashMap<>();
        final UserSchedule userSchedule = userEJB.getScheduleFor(user.getId(), null);
        for (UserScheduleConstraints userScheduleConstraints : userSchedule.getConstraints()) {
            constraints.put(userScheduleConstraints.getShowing().getId(), userScheduleConstraints.getConstraintType());
        }
    }

    public boolean isConstraintSelected(Long showingId, ScheduleConstraintType constraintType) {
        final ScheduleConstraintType scheduleConstraintType = constraints.get(showingId);
        return scheduleConstraintType != null && scheduleConstraintType == constraintType;
    }
}
