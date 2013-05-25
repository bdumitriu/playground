package org.ffplanner.bean;

import org.ffplanner.entity.Venue;
import org.ffplanner.entity.Venue_;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.persistence.metamodel.SingularAttribute;
import java.io.Serializable;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class VenueBean extends BasicEntityBean<Venue> implements Serializable {

    private static final long serialVersionUID = 1L;

    @Override
    protected Class<Venue> getEntityClass() {
        return Venue.class;
    }

    @Override
    protected SingularAttribute<Venue, Long> getIdAttribute() {
        return Venue_.id;
    }

    public Venue findOrCreateBy(String venueName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Venue> query = criteriaBuilder.createQuery(Venue.class);
        final Root<Venue> root = query.from(Venue.class);
        query.where(criteriaBuilder.equal(root.get(Venue_.name), venueName));
        final TypedQuery<Venue> venueQuery = entityManager.createQuery(query);
        final List<Venue> result = venueQuery.getResultList();
        final Venue venue;
        if (result.isEmpty()) {
            venue = new Venue(venueName);
            entityManager.persist(venue);
        } else {
            assert result.size() == 1;
            venue = result.get(0);
        }
        return venue;
    }
}
