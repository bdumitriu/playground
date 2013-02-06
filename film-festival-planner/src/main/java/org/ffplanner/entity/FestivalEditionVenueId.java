package org.ffplanner.entity;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
public class FestivalEditionVenueId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long festivalEdition;

    private Long venue;

    public FestivalEditionVenueId() {
    }

    public FestivalEditionVenueId(Long festivalEdition, Long venue) {
        this.festivalEdition = festivalEdition;
        this.venue = venue;
    }

    public Long getFestivalEdition() {
        return festivalEdition;
    }

    public Long getVenue() {
        return venue;
    }

    @Override
    public int hashCode() {
        return Objects.hash(festivalEdition, venue);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final FestivalEditionVenueId other = (FestivalEditionVenueId) obj;
        return Objects.equals(this.festivalEdition, other.festivalEdition) && Objects.equals(this.venue, other.venue);
    }
}
