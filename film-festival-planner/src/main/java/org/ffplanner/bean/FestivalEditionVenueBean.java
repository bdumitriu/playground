package org.ffplanner.bean;

import org.ffplanner.entity.*;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.io.Serializable;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class FestivalEditionVenueBean extends ConnectorEntityBean<FestivalEditionVenue> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Inject
    private FestivalEditionBean festivalEditionBean;

    @Inject
    private VenueBean venueBean;

    @Override
    protected Class<FestivalEditionVenue> getEntityClass() {
        return FestivalEditionVenue.class;
    }

    @Override
    protected Predicate getLeftCondition(Long id, CriteriaBuilder criteriaBuilder, Root<FestivalEditionVenue> root) {
        return criteriaBuilder.equal(root.get(FestivalEditionVenue_.festivalEdition).get(FestivalEdition_.id), id);
    }

    @Override
    protected Predicate getRightCondition(Long id, CriteriaBuilder criteriaBuilder, Root<FestivalEditionVenue> root) {
        return criteriaBuilder.equal(root.get(FestivalEditionVenue_.venue).get(Venue_.id), id);
    }

    @Override
    public FestivalEditionVenue find(Long festivalEditionId, Long venueId) {
        return super.find(festivalEditionId, venueId);
    }

    public FestivalEditionVenue findOrCreate(Long festivalEditionId, String venueName) {
        final Venue venue = venueBean.findOrCreateBy(venueName);
        FestivalEditionVenue festivalEditionVenue = find(festivalEditionId, venue.getId());
        if (festivalEditionVenue == null) {
            festivalEditionVenue = new FestivalEditionVenue();
            festivalEditionVenue.setFestivalEdition(festivalEditionBean.find(festivalEditionId));
            festivalEditionVenue.setVenue(venue);
            entityManager.persist(festivalEditionVenue);
        }
        return festivalEditionVenue;
    }
}
