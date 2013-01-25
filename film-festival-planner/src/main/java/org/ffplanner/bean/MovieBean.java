/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.Country;
import org.ffplanner.entity.Movie;
import org.ffplanner.entity.Person;

import javax.ejb.EJB;
import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBean {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @EJB
    private CountryBean countryBean;

    @EJB
    private PersonBean personBean;

    public void addMovie(Movie movie,
                         Iterable<String> directorNames, Iterable<String> actorNames, Iterable<String> countryNames) {
        final Collection<Country> countries = new ArrayList<>();
        for (String country : countryNames) {
            countries.add(countryBean.getCountry(country));
        }
        movie.addCountries(countries);
        final Collection<Person> actors = new ArrayList<>();
        for (String actorName : actorNames) {
            actors.add(personBean.addOrGetActor(actorName));
        }
        movie.addActors(actors);
        final Collection<Person> directors = new ArrayList<>();
        for (String directorName : directorNames) {
            directors.add(personBean.addOrGetDirector(directorName));
        }
        movie.addDirectors(directors);
        entityManager.persist(movie);
    }
}
