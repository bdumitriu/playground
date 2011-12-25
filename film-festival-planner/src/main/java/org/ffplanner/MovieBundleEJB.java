/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.ffplanner.entity.MovieBundle;
import org.ffplanner.entity.MovieBundle_;

/**
 * @author Bogdan Dumitriu
 */
@Stateless
@LocalBean
public class MovieBundleEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    @Inject
    private SectionEJB sectionEJB;

    @Inject
    private MovieEJB movieEJB;

    public void addShowing(MovieBundle movieBundle, String sectionName) {
        movieBundle.setSection(sectionEJB.getSection(sectionName));
        entityManager.persist(movieBundle);
    }
}
