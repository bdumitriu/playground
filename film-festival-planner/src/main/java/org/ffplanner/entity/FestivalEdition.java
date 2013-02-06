package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class FestivalEdition implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
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

    public void setStartDay(Date startDay) {
        this.startDay = startDay;
    }

    public Date getEndDay() {
        return endDay;
    }

    public void setEndDay(Date endDay) {
        this.endDay = endDay;
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
