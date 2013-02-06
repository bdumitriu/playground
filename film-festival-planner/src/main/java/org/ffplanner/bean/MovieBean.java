package org.ffplanner.bean;

import org.ffplanner.entity.Country;
import org.ffplanner.entity.Movie;
import org.ffplanner.entity.Movie_;
import org.ffplanner.entity.Person;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBean extends BasicEntityBean<Movie> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private CountryBean countryBean;

    @Inject
    private PersonBean personBean;

    @Override
    protected Class<Movie> getEntityClass() {
        return Movie.class;
    }

    @Override
    protected SingularAttribute<Movie, Long> getIdAttribute() {
        return Movie_.id;
    }

    public void createWith(
            Movie movie, Iterable<String> directorNames, Iterable<String> actorNames, Iterable<String> countryNames) {
        final Collection<Country> countries = new ArrayList<>();
        for (String country : countryNames) {
            countries.add(countryBean.findBy(country));
        }
        movie.addCountries(countries);
        final Collection<Person> actors = new ArrayList<>();
        for (String actorName : actorNames) {
            actors.add(personBean.findOrCreateActorBy(actorName));
        }
        movie.addActors(actors);
        final Collection<Person> directors = new ArrayList<>();
        for (String directorName : directorNames) {
            directors.add(personBean.findOrCreateDirectorBy(directorName));
        }
        movie.addDirectors(directors);
        entityManager.persist(movie);
    }
}
