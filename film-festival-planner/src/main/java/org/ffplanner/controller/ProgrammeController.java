/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.controller;

import org.ffplanner.bean.MovieEJB;
import org.ffplanner.bean.SectionEJB;
import org.ffplanner.entity.Movie;
import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.Section;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.inject.Named;
import java.io.Serializable;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Bogdan Dumitriu
 */
@Named
@RequestScoped
public class ProgrammeController implements Serializable {

    private final Logger log = Logger.getLogger(ProgrammeController.class.getName());

    @EJB
    private SectionEJB sectionEJB;

    @EJB
    private MovieEJB movieEJB;

    private List<Section> sections;

    private Section section;

    private MovieBundle movieBundle;

    private Movie movie;

    public List<Section> getSections() {
        if (sections == null) {
            sections = sectionEJB.getSections();
        }
        return sections;
    }

    public void setSection(Section section) {
        this.section = section;
    }

    public Section getSection() {
//        log.setLevel(Level.ALL);
        log.entering("ProgrammeController", "getSection");
        try {
            if (section == null) {
                final List<Section> sections = getSections();
                section = sections.isEmpty() ? null : sections.get(0);
            }
            return section;
        } finally {
            log.exiting("ProgrammeController", "getSection");
        }
    }

    public void setMovieBundle(MovieBundle movieBundle) {
        this.movieBundle = movieBundle;
    }

    public MovieBundle getMovieBundle() {
        log.entering("ProgrammeController", "getMovieBundle");
        try {
            if (movieBundle == null) {
                final Section section = getSection();
                if (section != null) {
                    final List<MovieBundle> movieBundles = section.getMovieBundles();
                    movieBundle = movieBundles.isEmpty() ? null : movieBundles.get(0);
                }
            }
            return movieBundle;
        } finally {
            log.exiting("ProgrammeController", "getMovieBundle");
        }
    }

    public Movie getMovie() {
        log.entering("ProgrammeController", "getMovie");
        try {
            if (movie == null) {
                final MovieBundle movieBundle = getMovieBundle();
                if (movieBundle != null) {
                    final List<Movie> movies = movieBundle.getMovies();
                    movie = movies.size() == 1 ? movies.get(0) : null;
                    if (movie != null) {
                        movie.getCountries().iterator();
                        movie.getDirectors().iterator();
                        movie.getActors().iterator();
                    }
                }
            }
            return movie;
        } finally {
            log.exiting("ProgrammeController", "getMovie");
        }
    }
}
