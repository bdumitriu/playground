package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "userschedule_showing")
@IdClass(UserScheduleShowingId.class)
public class UserScheduleShowing implements Serializable {

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

    @Override
    public int hashCode() {
        return Objects.hash(userScheduleId, showingId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final UserScheduleShowing other = (UserScheduleShowing) obj;
        return Objects.equals(this.userScheduleId, other.userScheduleId)
                && Objects.equals(this.showingId, other.showingId);
    }
}
