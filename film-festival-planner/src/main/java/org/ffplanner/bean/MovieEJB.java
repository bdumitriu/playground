/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.Country;
import org.ffplanner.entity.Movie;
import org.ffplanner.entity.Person;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @Inject
    private CountryEJB countryEJB;

    @Inject
    private PersonEJB personEJB;

    public void addMovie(Movie movie,
                         Iterable<String> directorNames, Iterable<String> actorNames, Iterable<String> countryNames) {
        final Collection<Country> countries = new ArrayList<>();
        for (String country : countryNames) {
            countries.add(countryEJB.getCountry(country));
        }
        movie.addCountries(countries);
        final Collection<Person> actors = new ArrayList<>();
        for (String actorName : actorNames) {
            actors.add(personEJB.addOrGetActor(actorName));
        }
        movie.addActors(actors);
        final Collection<Person> directors = new ArrayList<>();
        for (String directorName : directorNames) {
            directors.add(personEJB.addOrGetDirector(directorName));
        }
        movie.addDirectors(directors);
        entityManager.persist(movie);
    }
}
