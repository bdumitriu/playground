package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.text.ParseException;
import java.util.Date;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class ShowingBean extends BasicEntityBean<Showing> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalEditionVenueBean festivalEditionVenueBean;

    @Inject
    private MovieBundleInFestivalBean movieBundleInFestivalBean;

    @Override
    protected Class<Showing> getEntityClass() {
        return Showing.class;
    }

    @Override
    protected SingularAttribute<Showing, Long> getIdAttribute() {
        return Showing_.id;
    }

    /**
     * @param movieBundle
     *         the movie bundle being shown
     * @param day
     *         the day of the showing
     * @param time
     *         the time of the showing, expected in the format "16:00"
     * @param venueName
     *         where the showing takes places
     * @throws ParseException
     *         if the {@code duration} is not in the correct format
     */
    public void createWith(MovieBundle movieBundle, Long festivalEditionId, Date day, String time, String venueName)
            throws ParseException {
        final FestivalEditionVenue festivalEditionVenue =
                festivalEditionVenueBean.findOrCreate(festivalEditionId, venueName);
        final Showing showing = new Showing();
        showing.setDateAndTime(day, time);
        showing.setVenue(festivalEditionVenue.getVenue());
        final List<MovieBundleInFestival> movieBundlesInFestival =
                movieBundleInFestivalBean.findAll(movieBundle.getId(), festivalEditionId);
        if (movieBundlesInFestival.size() == 1) {
            showing.setMovieBundleInFestival(movieBundlesInFestival.get(0));
        } else {
            throw new RuntimeException("Not implemented: the same movie is shown under multiple sections.");
        }
        entityManager.persist(showing);
    }

    /**
     * @return all the movie showings of {@code festivalEdition}, sorted by venue and datetime.
     */
    public List<Showing> findBy(FestivalEdition festivalEdition) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Showing> query = criteriaBuilder.createQuery(Showing.class);
        final Root<Showing> showingRoot = query.from(Showing.class);
        final Join<Showing, MovieBundleInFestival> movieBundleInFestivalRoot =
                showingRoot.join(Showing_.movieBundleInFestival);
        query.where(criteriaBuilder.and(criteriaBuilder.equal(
                movieBundleInFestivalRoot.get(MovieBundleInFestival_.festivalEditionId), festivalEdition.getId())));
        query.orderBy(
                criteriaBuilder.asc(showingRoot.get(Showing_.venue)),
                criteriaBuilder.asc(showingRoot.get(Showing_.dateAndTime)));
        final TypedQuery<Showing> typedQuery = entityManager.createQuery(query);
        return typedQuery.getResultList();
    }
}
