/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class UserScheduleConstraintsId implements Serializable {

    private Long userScheduleId;

    private Long showingId;

    public UserScheduleConstraintsId() {
    }

    public UserScheduleConstraintsId(Long userScheduleId, Long showingId) {
        this.userScheduleId = userScheduleId;
        this.showingId = showingId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        UserScheduleConstraintsId that = (UserScheduleConstraintsId) o;

        if (showingId != null ? !showingId.equals(that.showingId) : that.showingId != null) {
            return false;
        }
        if (userScheduleId != null ? !userScheduleId.equals(that.userScheduleId) : that.userScheduleId != null) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        int result = userScheduleId != null ? userScheduleId.hashCode() : 0;
        result = 31 * result + (showingId != null ? showingId.hashCode() : 0);
        return result;
    }
}
