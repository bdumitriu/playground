/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class UserScheduleUseHistoryId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long user;

    private Long userSchedule;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        UserScheduleUseHistoryId that = (UserScheduleUseHistoryId) o;

        if (user != null ? !user.equals(that.user) : that.user != null) {
            return false;
        }
        if (userSchedule != null ? !userSchedule.equals(that.userSchedule) : that.userSchedule != null) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        int result = user != null ? user.hashCode() : 0;
        result = 31 * result + (userSchedule != null ? userSchedule.hashCode() : 0);
        return result;
    }
}
