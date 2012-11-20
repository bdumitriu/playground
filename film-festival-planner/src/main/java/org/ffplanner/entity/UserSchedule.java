/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
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
    private FestivalEdition festivalEdition;

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleShowings> showings = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleConstraints> constraints = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    @OrderBy("lastUsed desc")
    private List<UserScheduleUseHistory> useHistory = new LinkedList<>();

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

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public void setFestivalEdition(FestivalEdition festivalEdition) {
        this.festivalEdition = festivalEdition;
    }

    public Set<UserScheduleShowings> getShowings() {
        return showings;
    }

    public Set<UserScheduleConstraints> getConstraints() {
        return constraints;
    }

    public List<UserScheduleUseHistory> getUseHistory() {
        return useHistory;
    }
}
