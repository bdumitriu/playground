package org.ffplanner.entity;

import org.ffplanner.def.MovieDefinition;

import javax.persistence.*;
import java.io.Serializable;
import java.util.Objects;

/**
 * @author Bogdan Dumitriu
 */
@Entity
@Table(name = "moviebundle_festivaledition_section")
public class MovieBundleInFestival implements Serializable, MovieDefinition {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Long id;

    @Column(name = "movieBundle_id", insertable = false, updatable = false)
    private Long movieBundleId;

    @ManyToOne
    @JoinColumn(name = "movieBundle_id")
    private MovieBundle movieBundle;

    @Column(name = "festivalEdition_id", insertable = false, updatable = false)
    private Long festivalEditionId;

    @Column(name = "section_id", insertable = false, updatable = false)
    private Long sectionId;

    @ManyToOne
    @JoinColumns({@JoinColumn(name = "festivalEdition_id"), @JoinColumn(name = "section_id")})
    private FestivalEditionSection festivalEditionSection;

    public MovieBundleInFestival() {
    }

    public MovieBundleInFestival(MovieBundle movieBundle, FestivalEditionSection festivalEditionSection) {
        this.movieBundle = movieBundle;
        this.festivalEditionSection = festivalEditionSection;
    }

    public void loadLazyFields() {
        movieBundle.loadLazyFields();
    }

    @Override
    public Long getId() {
        return id;
    }

    public MovieBundle getMovieBundle() {
        return movieBundle;
    }

    public FestivalEditionSection getFestivalEditionSection() {
        return festivalEditionSection;
    }

    @Override
    public int hashCode() {
        return Objects.hash(movieBundleId, festivalEditionId, sectionId);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final MovieBundleInFestival other = (MovieBundleInFestival) obj;
        return Objects.equals(this.movieBundleId, other.movieBundleId)
                && Objects.equals(this.festivalEditionId, other.festivalEditionId)
                && Objects.equals(this.sectionId, other.sectionId);
    }
}
