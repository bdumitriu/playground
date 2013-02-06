package org.ffplanner.entity;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
public class UserScheduleShowingId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long userSchedule;

    private Long showing;

    public UserScheduleShowingId() {
    }

    public UserScheduleShowingId(Long userSchedule, Long showing) {
        this.userSchedule = userSchedule;
        this.showing = showing;
    }

    public Long getUserSchedule() {
        return userSchedule;
    }

    public Long getShowing() {
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
        final UserScheduleShowingId other = (UserScheduleShowingId) obj;
        return Objects.equals(this.userSchedule, other.userSchedule) && Objects.equals(this.showing, other.showing);
    }
}
