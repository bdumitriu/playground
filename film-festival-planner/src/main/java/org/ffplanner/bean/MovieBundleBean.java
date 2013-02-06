package org.ffplanner.bean;

import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.MovieBundle_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;

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
}
