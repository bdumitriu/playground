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
    private User user;

    @Id
    @ManyToOne
    @JoinColumn(name = "userschedule_id")
    private UserSchedule userSchedule;

    private Date lastUsed;

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public UserSchedule getUserSchedule() {
        return userSchedule;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userSchedule = userSchedule;
    }

    public Date getLastUsed() {
        return lastUsed;
    }

    public void setLastUsed(Date lastUsed) {
        this.lastUsed = lastUsed;
    }
}
