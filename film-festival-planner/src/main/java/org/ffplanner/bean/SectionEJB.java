/*
 * Copyright 2011 QTronic GmbH. All rights reserved.
 */
package org.ffplanner.bean;

import org.ffplanner.entity.Section;
import org.ffplanner.entity.Section_;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.util.List;

/**
 * @author Bogdan Dumitriu
 */
public class SectionEJB {

    @PersistenceContext(unitName = "ffp")
    private EntityManager entityManager;

    public Section getSection(String sectionName) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final CriteriaQuery<Section> query = criteriaBuilder.createQuery(Section.class);
        final Root<Section> root = query.from(Section.class);
        query.where(criteriaBuilder.equal(root.get(Section_.name), sectionName));
        final TypedQuery<Section> sectionQuery = entityManager.createQuery(query);
        final List<Section> result = sectionQuery.getResultList();
        final Section section;
        if (result.isEmpty()) {
            section = new Section(sectionName);
        } else {
            assert result.size() == 1;
            section = result.get(0);
        }
        return section;
    }
}
