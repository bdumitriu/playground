package org.ffplanner.entity;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
public class MovieBundleConstraintId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long userSchedule;

    private Long movieBundle;

    public MovieBundleConstraintId() {
    }

    public MovieBundleConstraintId(Long userScheduleId, Long movieBundleId) {
        this.userSchedule = userScheduleId;
        this.movieBundle = movieBundleId;
    }

    public Long getUserScheduleId() {
        return userSchedule;
    }

    public Long getMovieBundleId() {
        return movieBundle;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userSchedule, movieBundle);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final MovieBundleConstraintId other = (MovieBundleConstraintId) obj;
        return Objects.equals(this.userSchedule, other.userSchedule)
                && Objects.equals(this.movieBundle, other.movieBundle);
    }
}
