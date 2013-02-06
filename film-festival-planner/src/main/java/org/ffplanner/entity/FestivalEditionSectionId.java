package org.ffplanner.entity;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
public class FestivalEditionSectionId implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long festivalEdition;

    private Long section;

    public FestivalEditionSectionId() {
    }

    public FestivalEditionSectionId(Long festivalEdition, Long section) {
        this.festivalEdition = festivalEdition;
        this.section = section;
    }

    public Long getFestivalEditionId() {
        return festivalEdition;
    }

    public Long getSectionId() {
        return section;
    }

    @Override
    public int hashCode() {
        return Objects.hash(festivalEdition, section);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final FestivalEditionSectionId other = (FestivalEditionSectionId) obj;
        return Objects.equals(this.festivalEdition, other.festivalEdition)
                && Objects.equals(this.section, other.section);
    }
}
