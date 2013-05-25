package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "festivaledition_section")
@IdClass(FestivalEditionSectionId.class)
public class FestivalEditionSection implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column(name = "festivalEdition_id", insertable = false, updatable = false)
    private Long festivalEditionId;

    @Id
    @ManyToOne
    @JoinColumn(name = "festivalEdition_id")
    private FestivalEdition festivalEdition;

    @Column(name = "section_id", insertable = false, updatable = false)
    private Long sectionId;

    @Id
    @ManyToOne
    @JoinColumn(name = "section_id")
    private Section section;

    private Integer sectionOrder;

    @OneToMany(mappedBy = "festivalEditionSection")
    private List<MovieBundleInFestival> movieBundlesInFestival;

    public void loadLazyFields() {
        for (MovieBundleInFestival movieBundleInFestival : movieBundlesInFestival) {
            movieBundleInFestival.loadLazyFields();
        }
    }

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public void setFestivalEdition(FestivalEdition festivalEdition) {
        this.festivalEdition = festivalEdition;
    }

    public Section getSection() {
        return section;
    }

    public void setSection(Section section) {
        this.section = section;
    }

    public Integer getSectionOrder() {
        return sectionOrder;
    }

    public void setSectionOrder(Integer sectionOrder) {
        this.sectionOrder = sectionOrder;
    }

    public List<MovieBundleInFestival> getMovieBundles() {
        return movieBundlesInFestival;
    }

    @Override
    public int hashCode() {
        return Objects.hash(festivalEditionId, sectionId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final FestivalEditionSection other = (FestivalEditionSection) obj;
        return Objects.equals(this.festivalEditionId, other.festivalEditionId)
                && Objects.equals(this.sectionId, other.sectionId);
    }
}
