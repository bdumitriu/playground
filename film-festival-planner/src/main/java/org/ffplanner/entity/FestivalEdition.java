package org.ffplanner.entity;

import org.ffplanner.helper.Day;
import org.joda.time.DateTime;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "festivaledition")
public class FestivalEdition implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "festival_id", insertable = false, updatable = false)
    private Long festivalId;

    @ManyToOne
    @JoinColumn(name = "festival_id")
    private Festival festival;

    private Integer editionNumber;

    private String editionName;

    private Date startDay;

    private Date endDay;

    @Transient
    private List<Day> days;

    @OneToMany(mappedBy = "festivalEdition")
    @OrderBy("sectionOrder")
    private List<FestivalEditionSection> festivalEditionSections = new LinkedList<>();

    @OneToMany(mappedBy = "festivalEdition")
    @OrderBy("venueOrder")
    private List<FestivalEditionVenue> festivalEditionVenues = new LinkedList<>();

    public void loadLazyFields() {
        loadSections();
        loadVenues();
    }

    public void loadSections() {
        for (FestivalEditionSection festivalEditionSection : festivalEditionSections) {
            festivalEditionSection.loadLazyFields();
        }
    }

    public void loadVenues() {
        festivalEditionVenues.iterator();
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Festival getFestival() {
        return festival;
    }

    public void setFestival(Festival festival) {
        this.festival = festival;
    }

    public Integer getEditionNumber() {
        return editionNumber;
    }

    public void setEditionNumber(Integer editionNumber) {
        this.editionNumber = editionNumber;
    }

    public String getEditionName() {
        return editionName;
    }

    public void setEditionName(String editionName) {
        this.editionName = editionName;
    }

    public Date getStartDay() {
        return startDay;
    }

    public synchronized void setStartDay(Date startDay) {
        this.startDay = startDay;
        this.days = null;
    }

    public Date getEndDay() {
        return endDay;
    }

    public synchronized void setEndDay(Date endDay) {
        this.endDay = endDay;
        this.days = null;
    }

    public synchronized List<Day> getDays() {
        if (this.days == null) {
            this.days = new LinkedList<>();
            DateTime dateTime = new DateTime(startDay);
            final DateTime endDateTime = new DateTime(endDay);
            if (startDay == null || endDay == null || dateTime.isAfter(endDateTime)) {
                this.days = Collections.emptyList();
            } else {
                do {
                    this.days.add(new Day(dateTime));
                    dateTime = dateTime.plusDays(1);
                } while (!dateTime.isAfter(endDateTime));
            }
        }
        return this.days;
    }

    public List<FestivalEditionSection> getSections() {
        return festivalEditionSections;
    }

    public List<FestivalEditionVenue> getVenues() {
        return festivalEditionVenues;
    }

    @Override
    public int hashCode() {
        return Objects.hash(festivalId, editionNumber);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final FestivalEdition other = (FestivalEdition) obj;
        return Objects.equals(this.festivalId, other.festivalId)
                && Objects.equals(this.editionNumber, other.editionNumber);
    }
}
