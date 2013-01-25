/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.Movie;
import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.MovieBundle_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.metamodel.SingularAttribute;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBundleBean extends BasicEntityBean<MovieBundle> {

    @Inject
    private SectionBean sectionBean;

    @Inject
    private MovieBean movieBean;

    @Override
    protected Class<MovieBundle> getEntityClass() {
        return MovieBundle.class;
    }

    @Override
    protected SingularAttribute<MovieBundle, Long> getIdAttribute() {
        return MovieBundle_.id;
    }

    @Override
    public MovieBundle find(Long id) {
        final MovieBundle movieBundle = super.find(id);
        if (movieBundle != null) {
            forceLazyLoad(movieBundle);
        }
        return movieBundle;
    }

    public void addShowing(MovieBundle movieBundle, String sectionName) {
        movieBundle.setSection(sectionBean.findBy(sectionName));
        entityManager.persist(movieBundle);
    }

    public static void forceLazyLoad(MovieBundle movieBundle) {
        for (Movie movie : movieBundle.getMovies()) {
            movie.getActors().iterator();
            movie.getDirectors().iterator();
            movie.getCountries().iterator();
        }
    }
}