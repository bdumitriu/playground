package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

import static org.ffplanner.entity.QueryName.USER_SCHEDULE_SHOWING__DELETE_SCHEDULES_BY_USER_SCHEDULE_ID;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@NamedQuery(
        name = USER_SCHEDULE_SHOWING__DELETE_SCHEDULES_BY_USER_SCHEDULE_ID,
        query = "delete from UserScheduleShowing where userScheduleId = :userScheduleId"
)
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

    @Id
    @Column(name = "proposal_id")
    private Long proposalId;

    @Column(name = "showing_id", insertable = false, updatable = false)
    private Long showingId;

    @Id
    @ManyToOne
    @JoinColumn(name = "showing_id")
    private Showing showing;

    public UserSchedule getUserSchedule() {
        return userSchedule;
    }

    public void setUserSchedule(UserSchedule userSchedule) {
        this.userSchedule = userSchedule;
    }

    public Long getUserScheduleId() {
        return userScheduleId;
    }

    public Long getProposalId() {
        return proposalId;
    }

    public void setProposalId(Long proposalId) {
        this.proposalId = proposalId;
    }

    public void setShowing(Showing showing) {
        this.showing = showing;
    }

    public Long getShowingId() {
        return showingId;
    }

    @Override
    public int hashCode() {
        return Objects.hash(userScheduleId, proposalId, showingId);
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
                && Objects.equals(this.proposalId, other.proposalId) && Objects.equals(this.showingId, other.showingId);
    }
}
