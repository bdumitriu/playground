/*
 * Copyright 2012 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
public class UserScheduleShowingsId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long userSchedule;

    private Long showing;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        UserScheduleShowingsId that = (UserScheduleShowingsId) o;

        if (showing != null ? !showing.equals(that.showing) : that.showing != null) {
            return false;
        }
        if (userSchedule != null ? !userSchedule.equals(that.userSchedule) : that.userSchedule != null) {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode() {
        int result = userSchedule != null ? userSchedule.hashCode() : 0;
        result = 31 * result + (showing != null ? showing.hashCode() : 0);
        return result;
    }
}
