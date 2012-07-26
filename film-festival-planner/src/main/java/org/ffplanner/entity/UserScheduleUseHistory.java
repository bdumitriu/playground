/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;

/**
 * @author Bogdan Dumitriu
 */
@Entity(name = "ScheduleUseHistory")
@IdClass(UserScheduleUseHistoryId.class)
public class UserScheduleUseHistory implements Serializable {

    @Id
    @ManyToOne
    @JoinColumn(name = "user_id")
    private User userId;

    @Id
    @ManyToOne
    @JoinColumn(name = "userschedule_id")
    private UserSchedule userScheduleId;

    private Date lastUsed;

    public User getUser() {
        return userId;
    }

    public void setUser(User user) {
        this.userId = user;
    }

    public UserSchedule getUserSchedule() {
        return userScheduleId;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userScheduleId = userSchedule;
    }

    public Date getLastUsed() {
        return lastUsed;
    }

    public void setLastUsed(Date lastUsed) {
        this.lastUsed = lastUsed;
    }
}
