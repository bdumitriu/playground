package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "moviebundle_constraint")
@IdClass(MovieBundleConstraintId.class)
public class MovieBundleConstraint implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column(name = "userSchedule_id", insertable = false, updatable = false)
    private Long userScheduleId;

    @Id
    @ManyToOne
    @JoinColumn(name = "userSchedule_id")
    private UserSchedule userSchedule;

    @Column(name = "movieBundle_id", insertable = false, updatable = false)
    private Long movieBundleId;

    @Id
    @ManyToOne
    @JoinColumn(name = "movieBundle_id")
    private MovieBundleInFestival movieBundle;

    private Short priority;

    public UserSchedule getUserSchedule() {
        return userSchedule;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userSchedule = userSchedule;
    }

    public MovieBundleInFestival getMovieBundle() {
        return movieBundle;
    }

    public void setMovieBundle(MovieBundleInFestival movieBundle) {
        this.movieBundle = movieBundle;
    }

    public Short getPriority() {
        return priority;
    }

    public void setPriority(Short priority) {
        this.priority = priority;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userScheduleId, movieBundleId, priority);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final MovieBundleConstraint other = (MovieBundleConstraint) obj;
        return Objects.equals(this.userScheduleId, other.userScheduleId)
                && Objects.equals(this.movieBundleId, other.movieBundleId)
                && Objects.equals(this.priority, other.priority);
    }
}
