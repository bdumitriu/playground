/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class User implements Serializable {

    private static final long serialVersionUID = -3332755932464496941L;

    @Id
    @GeneratedValue
    private Long id;

    private String firstName;

    private String lastName;

    private String emailAddress;

    @OneToMany(mappedBy = "userId")
    @OrderBy("lastUsed desc")
    private List<UserScheduleUseHistory> schedules = new LinkedList<>();

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(String emailAddress) {
        this.emailAddress = emailAddress;
    }

    public List<UserScheduleUseHistory> getSchedules() {
        return schedules;
    }

    public void addSchedule(UserScheduleUseHistory schedule) {
        this.schedules.add(schedule);
    }
}
