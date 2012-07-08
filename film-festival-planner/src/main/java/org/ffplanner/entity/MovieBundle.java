/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import org.ffplanner.util.DateUtils;

import javax.persistence.*;
import java.io.Serializable;
import java.util.*;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class MovieBundle implements Serializable {

    private static final long serialVersionUID = 1L;

    @GeneratedValue
    @Id
    private Long id;

    private String englishTitle;

    private String originalTitle;

    @ManyToOne(cascade = CascadeType.ALL)
    private Section section;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @OrderBy
    private List<Movie> movies;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEnglishTitle() {
        return movies.size() == 1 ? movies.iterator().next().getEnglishTitle() : englishTitle;
    }

    public void setEnglishTitle(String englishTitle) {
        this.englishTitle = englishTitle;
    }

    public String getOriginalTitle() {
        return movies.size() == 1 ? movies.iterator().next().getOriginalTitle() : originalTitle;
    }

    public void setOriginalTitle(String originalTitle) {
        this.originalTitle = originalTitle;
    }

    public Section getSection() {
        return section;
    }

    public void setSection(Section section) {
        this.section = section;
    }

    public void addMovies(Collection<Movie> movies) {
        if (this.movies == null) {
            this.movies = new LinkedList<>();
        }
        this.movies.addAll(movies);
    }

    public List<Movie> getMovies() {
        return movies;
    }

    public String getFormattedDuration() {
        return new DateUtils().formatHoursAndMinutes(getDurationInMinutes());
    }

    public int getDurationInMinutes() {
        int totalDuration = 0;
        for (Movie movie : movies) {
            totalDuration += movie.getDurationInMinutes();
        }
        return totalDuration;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        MovieBundle that = (MovieBundle) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    @Override
    public String toString() {
        return "MovieBundle{" +
                "englishTitle='" + englishTitle + '\'' +
                ", originalTitle='" + originalTitle + '\'' +
                ", section=" + section +
                ", movies=" + movies +
                '}';
    }
}
