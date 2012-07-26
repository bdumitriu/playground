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

    private Long userId;

    private Long userScheduleId;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        UserScheduleUseHistoryId that = (UserScheduleUseHistoryId) o;

        if (userId != null ? !userId.equals(that.userId) : that.userId != null) {
            return false;
        }
        if (userScheduleId != null ? !userScheduleId.equals(that.userScheduleId) : that.userScheduleId != null) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        int result = userId != null ? userId.hashCode() : 0;
        result = 31 * result + (userScheduleId != null ? userScheduleId.hashCode() : 0);
        return result;
    }
}
