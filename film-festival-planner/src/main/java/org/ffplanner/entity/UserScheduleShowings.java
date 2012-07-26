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
@IdClass(UserScheduleShowingsId.class)
public class UserScheduleShowings implements Serializable {

    @Id
    @ManyToOne
    @JoinColumn(name = "userschedule_id")
    private UserSchedule userScheduleId;

    @Id
    @ManyToOne
    @JoinColumn(name = "showing_id")
    private Showing showingId;
}
