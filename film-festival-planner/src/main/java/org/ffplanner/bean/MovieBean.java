package org.ffplanner.bean;

import org.ffplanner.entity.*;
import org.ffplanner.util.ConstantsToGetRidOf;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

    /**
     * @return all the movies in {@code festivalEdition}, sorted by id.
     */
    public List<MovieBundleInFestival> findBy(FestivalEdition festivalEdition) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<MovieBundleInFestival> query = criteriaBuilder.createQuery(MovieBundleInFestival.class);
        final Root<MovieBundleInFestival> root = query.from(MovieBundleInFestival.class);
        query.where(criteriaBuilder.equal(root.get(MovieBundleInFestival_.festivalEditionSection).get(FestivalEditionSection_.festivalEdition).get(FestivalEdition_.id), ConstantsToGetRidOf.DEFAULT_FESTIVAL_EDITION_ID));
        query.orderBy(criteriaBuilder.asc(root.get(MovieBundleInFestival_.id)));
        final TypedQuery<MovieBundleInFestival> typedQuery = entityManager.createQuery(query);
        return typedQuery.getResultList();
    }
}
