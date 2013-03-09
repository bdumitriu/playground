package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "showing_constraint")
@IdClass(ShowingConstraintId.class)
public class ShowingConstraint implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column(name = "userSchedule_id", insertable = false, updatable = false)
    private Long userScheduleId;

    @Id
    @ManyToOne
    @JoinColumn(name = "userSchedule_id")
    private UserSchedule userSchedule;

    @Column(name = "showing_id", insertable = false, updatable = false)
    private Long showingId;

    @Id
    @ManyToOne
    @JoinColumn(name = "showing_id")
    private Showing showing;

    private Short priority;

    public UserSchedule getUserSchedule() {
        return userSchedule;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userSchedule = userSchedule;
    }

    public Showing getShowing() {
        return showing;
    }

    public void setShowing(Showing showing) {
        this.showing = showing;
    }

    public Short getPriority() {
        return priority;
    }

    public void setPriority(Short priority) {
        this.priority = priority;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userScheduleId, showingId, priority);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final ShowingConstraint other = (ShowingConstraint) obj;
        return Objects.equals(this.userScheduleId, other.userScheduleId)
                && Objects.equals(this.showingId, other.showingId)
                && Objects.equals(this.priority, other.priority);
    }
}
