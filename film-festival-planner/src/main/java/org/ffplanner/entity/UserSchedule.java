/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class UserSchedule implements Serializable {

    @Id
    @GeneratedValue
    private Long id;

    private String scheduleName;

    @ManyToOne
    private User user;

    @ManyToOne
    private FestivalEdition festivalEdition;

    private Date lastUsed;

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleShowings> showings = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleConstraints> constraints = new HashSet<>();

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getScheduleName() {
        return scheduleName;
    }

    public void setScheduleName(String scheduleName) {
        this.scheduleName = scheduleName;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public void setFestivalEdition(FestivalEdition festivalEdition) {
        this.festivalEdition = festivalEdition;
    }

    public Date getLastUsed() {
        return lastUsed;
    }

    public void setLastUsed(Date lastUsed) {
        this.lastUsed = lastUsed;
    }

    public Set<UserScheduleShowings> getShowings() {
        return showings;
    }

    public Set<UserScheduleConstraints> getConstraints() {
        return constraints;
    }
}
