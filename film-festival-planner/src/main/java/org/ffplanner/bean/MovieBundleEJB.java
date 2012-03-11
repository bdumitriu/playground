/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.MovieBundle;

import javax.ejb.LocalBean;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

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
