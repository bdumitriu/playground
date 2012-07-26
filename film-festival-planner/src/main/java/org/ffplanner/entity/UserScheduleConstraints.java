/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@IdClass(UserScheduleConstraintsId.class)
public class UserScheduleConstraints implements Serializable {

    @Id
    @ManyToOne
    @JoinColumn(name = "userschedule_id")
    private UserSchedule userScheduleId;

    @Id
    @ManyToOne
    @JoinColumn(name = "showing_id")
    private Showing showingId;

    @Enumerated(EnumType.STRING)
    private ScheduleConstraintType constraintType;

    public UserSchedule getUserSchedule() {
        return userScheduleId;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userScheduleId = userSchedule;
    }

    public Showing getShowing() {
        return showingId;
    }

    public void setShowing(Showing showing) {
        this.showingId = showing;
    }

    public ScheduleConstraintType getConstraintType() {
        return constraintType;
    }

    public void setConstraintType(ScheduleConstraintType constraintType) {
        this.constraintType = constraintType;
    }
}
