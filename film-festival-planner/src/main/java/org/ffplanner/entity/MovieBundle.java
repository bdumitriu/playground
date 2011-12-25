/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.entity;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Entity
public class MovieBundle {

    @GeneratedValue
    @Id
    private Long id;

    private String englishTitle;

    private String originalTitle;

    @ManyToOne(cascade = CascadeType.ALL)
    private Section section;

    @ManyToMany
    private List<Movie> movies;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEnglishTitle() {
        return movies.size() == 1 ? movies.get(0).getEnglishTitle() : englishTitle;
    }

    public void setEnglishTitle(String englishTitle) {
        this.englishTitle = englishTitle;
    }

    public String getOriginalTitle() {
        return movies.size() == 1 ? movies.get(0).getOriginalTitle() : originalTitle;
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
            this.movies = new ArrayList<>();
        }
        this.movies.addAll(movies);
    }

    public List<Movie> getMovies() {
        return movies;
    }

    public int getDurationInMinutes() {
        int totalDuration = 0;
        for (Movie movie : movies) {
            totalDuration += movie.getDurationInMinutes();
        }
        return totalDuration;
    }
}
