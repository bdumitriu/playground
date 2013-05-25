package org.ffplanner.entity;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "festivaledition_venue")
@IdClass(FestivalEditionVenueId.class)
public class FestivalEditionVenue implements Serializable {

    private static final long serialVersionUID = 1L;

    @Column(name = "festivalEdition_id", insertable = false, updatable = false)
    private Long festivalEditionId;

    @Id
    @ManyToOne
    @JoinColumn(name = "festivalEdition_id")
    private FestivalEdition festivalEdition;

    @Column(name = "venue_id", insertable = false, updatable = false)
    private Long venueId;

    @Id
    @ManyToOne
    @JoinColumn(name = "venue_id")
    private Venue venue;

    private Integer venueOrder;

    public FestivalEdition getFestivalEdition() {
        return festivalEdition;
    }

    public void setFestivalEdition(FestivalEdition festivalEdition) {
        this.festivalEdition = festivalEdition;
    }

    public Venue getVenue() {
        return venue;
    }

    public void setVenue(Venue venue) {
        this.venue = venue;
    }

    public Integer getVenueOrder() {
        return venueOrder;
    }

    public void setVenueOrder(Integer venueOrder) {
        this.venueOrder = venueOrder;
    }

    @Override
    public int hashCode() {
        return Objects.hash(festivalEditionId, venueId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final FestivalEditionVenue other = (FestivalEditionVenue) obj;
        return Objects.equals(this.festivalEditionId, other.festivalEditionId)
                && Objects.equals(this.venueId, other.venueId);
    }
}
