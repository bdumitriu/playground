package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "userschedule_constraint")
@IdClass(UserScheduleConstraintId.class)
public class UserScheduleConstraint implements Serializable {

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

    @Enumerated(EnumType.STRING)
    private ScheduleConstraintType constraintType;

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

    public ScheduleConstraintType getConstraintType() {
        return constraintType;
    }

    public void setConstraintType(ScheduleConstraintType constraintType) {
        this.constraintType = constraintType;
    }

    public Short getPriority() {
        return priority;
    }

    public void setPriority(Short priority) {
        this.priority = priority;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userScheduleId, showingId, constraintType, priority);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final UserScheduleConstraint other = (UserScheduleConstraint) obj;
        return Objects.equals(this.userScheduleId, other.userScheduleId)
                && Objects.equals(this.showingId, other.showingId)
                && Objects.equals(this.constraintType, other.constraintType)
                && Objects.equals(this.priority, other.priority);
    }
}
