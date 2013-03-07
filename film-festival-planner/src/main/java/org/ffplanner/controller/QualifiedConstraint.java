package org.ffplanner.controller;

import org.ffplanner.def.ConstraintDefinition;
import org.ffplanner.entity.ScheduleConstraintType;

/**
 * @author Bogdan Dumitriu
 */
public class QualifiedConstraint implements ConstraintDefinition {

    private ScheduleConstraintType scheduleConstraintType;

    private Short priority;

    public QualifiedConstraint(ScheduleConstraintType scheduleConstraintType, Short priority) {
        this.scheduleConstraintType = scheduleConstraintType;
        this.priority = priority;
    }

    public ScheduleConstraintType getScheduleConstraintType() {
        return scheduleConstraintType;
    }

    public void setScheduleConstraintType(ScheduleConstraintType scheduleConstraintType) {
        this.scheduleConstraintType = scheduleConstraintType;
    }

    @Override
    public WatchType getWatchType() {
        return scheduleConstraintType.getWatchType();
    }

    @Override
    public Short getPriority() {
        return priority;
    }

    public void setPriority(Short priority) {
        this.priority = priority;
    }
}
