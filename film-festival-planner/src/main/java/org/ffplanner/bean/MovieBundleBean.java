package org.ffplanner.bean;

import org.ffplanner.entity.Movie;
import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.MovieBundle_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedList;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBundleBean extends BasicEntityBean<MovieBundle> implements Serializable {

    private static final long serialVersionUID = 1L;

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
            movieBundle.loadLazyFields();
        }
        return movieBundle;
    }

    public void createWith(MovieBundle movieBundle, Iterable<Long> movieIds) {
        final Collection<Movie> movieReferences = new LinkedList<>();
        for (Long movieId : movieIds) {
            final Movie movieReference = movieBean.getReference(movieId);
            if (movieReference == null) {
                throw new RuntimeException("Not implemented: the movie was expected to be in the database already.");
            } else {
                movieReferences.add(movieReference);
            }
        }
        movieBundle.addMovies(movieReferences);
        entityManager.persist(movieBundle);
    }
}
