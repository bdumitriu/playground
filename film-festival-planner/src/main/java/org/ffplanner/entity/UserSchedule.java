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
@Table(name = "userschedule")
public class UserSchedule implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
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

    private Date constraintsLastModified;

    private Date scheduleLastModified;

    @OneToMany(mappedBy = "userSchedule")
    private Set<UserScheduleShowing> showings = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    private Set<MovieBundleConstraint> movieConstraints = new HashSet<>();

    @OneToMany(mappedBy = "userSchedule")
    private Set<ShowingConstraint> showingConstraints = new HashSet<>();

    public void loadLazyFields() {
        showings.iterator();
        movieConstraints.iterator();
        showingConstraints.iterator();
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

    public Date getConstraintsLastModified() {
        return constraintsLastModified;
    }

    public void setConstraintsLastModified(Date constraintsLastModified) {
        this.constraintsLastModified = constraintsLastModified;
    }

    public Date getScheduleLastModified() {
        return scheduleLastModified;
    }

    public void setScheduleLastModified(Date scheduleLastModified) {
        this.scheduleLastModified = scheduleLastModified;
    }

    public Set<UserScheduleShowing> getShowings() {
        return showings;
    }

    public Set<MovieBundleConstraint> getMovieConstraints() {
        return movieConstraints;
    }

    public Set<ShowingConstraint> getShowingConstraints() {
        return showingConstraints;
    }

    public void resetConstraints() {
        movieConstraints.clear();
        showingConstraints.clear();
    }

    public void resetSchedule() {
        showings.clear();
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
