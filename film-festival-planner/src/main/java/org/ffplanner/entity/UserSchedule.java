package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class UserSchedule implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Long id;

    private String scheduleName;

    @Column(name = "user_id", insertable = false, updatable = false)
    private Long userId;

    @ManyToOne
    private User user;

    @Column(name = "festivalEdition_id", insertable = false, updatable = false)
    private Long festivalEditionId;

    @ManyToOne
    private FestivalEdition festivalEdition;

    private Date lastUsed;

    private Date lastModified;

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleShowing> showings = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleConstraint> constraints = new HashSet<>();

    public void loadLazyFields() {
        showings.iterator();
        constraints.iterator();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getScheduleName() {
        return scheduleName;
    }

    public void setScheduleName(String scheduleName) {
        this.scheduleName = scheduleName;
    }

    public User getUser() {
        return user;
    }

    public void setUser(User user) {
        this.user = user;
    }

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public void setFestivalEdition(FestivalEdition festivalEdition) {
        this.festivalEdition = festivalEdition;
    }

    public Date getLastUsed() {
        return lastUsed;
    }

    public void setLastUsed(Date lastUsed) {
        this.lastUsed = lastUsed;
    }

    public Date getLastModified() {
        return lastModified;
    }

    public void setLastModified(Date lastModified) {
        this.lastModified = lastModified;
    }

    public Set<UserScheduleShowing> getShowings() {
        return showings;
    }

    public Set<UserScheduleConstraint> getConstraints() {
        return constraints;
    }

    @Override
    public int hashCode() {
        return Objects.hash(scheduleName, userId, festivalEditionId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final UserSchedule other = (UserSchedule) obj;
        return Objects.equals(this.scheduleName, other.scheduleName) && Objects.equals(this.userId, other.userId)
                && Objects.equals(this.festivalEditionId, other.festivalEditionId);
    }
}
