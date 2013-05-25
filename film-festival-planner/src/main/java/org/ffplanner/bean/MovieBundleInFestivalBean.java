package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.NoResultException;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBundleInFestivalBean extends EntityBean<MovieBundleInFestival> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private MovieBundleBean movieBundleBean;

    @Inject
    private SectionBean sectionBean;

    @Inject
    private FestivalEditionSectionBean festivalEditionSectionBean;

    @Override
    protected Class<MovieBundleInFestival> getEntityClass() {
        return MovieBundleInFestival.class;
    }

    public MovieBundleInFestival find(Long movieBundleInFestivalId) {
        return entityManager.find(MovieBundleInFestival.class, movieBundleInFestivalId);
    }

    public MovieBundleInFestival find(Long movieBundleId, Long festivalEditionId, Long sectionId) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<MovieBundleInFestival> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<MovieBundleInFestival> root = query.from(getEntityClass());
        query.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(MovieBundleInFestival_.movieBundle).get(MovieBundle_.id), movieBundleId),
                criteriaBuilder.and(
                        criteriaBuilder.equal(root.get(MovieBundleInFestival_.festivalEditionSection)
                                .get(FestivalEditionSection_.festivalEdition)
                                .get(FestivalEdition_.id), festivalEditionId),
                        criteriaBuilder.equal(root.get(MovieBundleInFestival_.festivalEditionSection)
                                .get(FestivalEditionSection_.section)
                                .get(Section_.id), sectionId))));
        final TypedQuery<MovieBundleInFestival> typedQuery = entityManager.createQuery(query);
        try {
            return typedQuery.getSingleResult();
        } catch (NoResultException ignored) {
            return null;
        }
    }

    public List<MovieBundleInFestival> findAll(Long movieBundleId, Long festivalEditionId) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<MovieBundleInFestival> query = criteriaBuilder.createQuery(getEntityClass());
        final Root<MovieBundleInFestival> root = query.from(getEntityClass());
        query.where(criteriaBuilder.and(
                criteriaBuilder.equal(root.get(MovieBundleInFestival_.movieBundle).get(MovieBundle_.id), movieBundleId),
                criteriaBuilder.equal(root.get(MovieBundleInFestival_.festivalEditionSection)
                        .get(FestivalEditionSection_.festivalEdition)
                        .get(FestivalEdition_.id), festivalEditionId)));
        final TypedQuery<MovieBundleInFestival> typedQuery = entityManager.createQuery(query);
        try {
            return typedQuery.getResultList();
        } catch (NoResultException ignored) {
            return null;
        }
    }

    public void addMovieBundleInFestival(Long movieBundleId, Long festivalEditionId, String sectionName) {
        final MovieBundle movieBundleReference = movieBundleBean.getReference(movieBundleId);
        if (movieBundleReference == null) {
            throw new RuntimeException("Not implemented: the movie bundle was expected to be in the database already.");
        } else {
            final MovieBundleInFestival movieBundleInFestival = new MovieBundleInFestival(movieBundleReference,
                    festivalEditionSectionBean.findOrCreate(festivalEditionId, sectionName));
            entityManager.persist(movieBundleInFestival);
        }
    }
}
