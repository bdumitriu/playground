/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

import org.ffplanner.entity.Venue;
import org.ffplanner.entity.Venue_;

/**
 * @author Bogdan Dumitriu
 */
public class VenueEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    public Venue getVenue(String venueName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Venue> query = criteriaBuilder.createQuery(Venue.class);
        final Root<Venue> root = query.from(Venue.class);
        query.where(criteriaBuilder.equal(root.get(Venue_.name), venueName));
        final TypedQuery<Venue> venueQuery = entityManager.createQuery(query);
        final List<Venue> result = venueQuery.getResultList();
        final Venue venue;
        if (result.isEmpty()) {
            venue = new Venue(venueName);
        } else {
            assert result.size() == 1;
            venue = result.get(0);
        }
        return venue;
    }
}
