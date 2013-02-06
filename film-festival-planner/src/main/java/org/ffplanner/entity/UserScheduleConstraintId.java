package org.ffplanner.entity;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
public class UserScheduleConstraintId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long userSchedule;

    private Long showing;

    public UserScheduleConstraintId() {
    }

    public UserScheduleConstraintId(Long userScheduleId, Long showingId) {
        this.userSchedule = userScheduleId;
        this.showing = showingId;
    }

    public Long getUserScheduleId() {
        return userSchedule;
    }

    public Long getShowingId() {
        return showing;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userSchedule, showing);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final UserScheduleConstraintId other = (UserScheduleConstraintId) obj;
        return Objects.equals(this.userSchedule, other.userSchedule) && Objects.equals(this.showing, other.showing);
    }
}
